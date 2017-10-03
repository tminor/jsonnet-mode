;;; jsonnet-mode.el --- Major mode for editing jsonnet files

;; Copyright (C) 2017 Nick Lanham

;; Author: Nick Lanham
;; URL: https://github.com/mgyucht/jsonnet-mode
;; Package-Version: 0.0.1
;; Keywords: languages
;; Package-Requires: ((emacs "24"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides syntax highlighting, indenting, formatting, and utility
;; methods for jsonnet files. To use it, place it somewhere in your load-path,
;; and then add the following to your init.el:
;; (load "jsonnet-mode")
;;
;; This mode creates the following keybindings:
;;   'C-c C-c' evaluates the current buffer in Jsonnet and put the output in an
;;             output buffer
;;   'C-c C-f' jumps to the definition of the identifier at point
;;   'C-c C-r' reformats the entire buffer using Jsonnet's fmt utility

;;; Code:

(defgroup jsonnet '()
  "Major mode for editing Jsonnet files."
  :group 'languages)

(defcustom jsonnet-command
  "jsonnet"
  "Jsonnet command to run in ‘jsonnet-eval’."
  :type '(string)
  :group 'jsonnet)

(defcustom jsonnet-enable-debug-print
  nil
  "If non-nil, enables debug printing in ‘jsonnet-mode’ functions."
  :type '(boolean)
  :group 'jsonnet)

(defconst jsonnet--identifier-regexp
  "[a-zA-Z_][a-zA-Z0-9_]*"
  "Regular expression matching a Jsonnet identifier.")

(defconst jsonnet--function-name-regexp
  (concat "local \\(" jsonnet--identifier-regexp "\\)"
          "\\(([a-zA-Z0-9_, ]*)\s*=\\|\s*=\s*function\\)"))

(defconst jsonnet-font-lock-keywords-1
  (let ((builtin-regex (regexp-opt '("assert" "else" "error" "for" "function" "if" "import" "importstr" "in" "local" "self" "super" "then") 'words))
        (constant-regex (regexp-opt '("false" "null" "true") 'words))
        (function-name-regex jsonnet--function-name-regexp)
        ;; Any other local bindings are variables
        (variable-name-regex (concat "local \\(" jsonnet--identifier-regexp "\\)\s+="))
        ;; All standard library functions (see https://jsonnet.org/docs/stdlib.html)
        (standard-functions-regex (regexp-opt (mapcar (lambda (func-name) (concat "std." func-name))
                                                '("abs" "acos" "asin" "assertEqual" "atan" "base64" "base64Decode" "base64DecodeBytes" "ceil" "char" "codepoint" "cos" "count" "endsWith" "escapeStringBash" "escapeStringDollars" "escapeStringJson" "escapeStringPython" "exp" "exponent" "extVar" "filter" "filterMap" "flattenArrays" "floor" "foldl" "foldr" "format" "join" "length" "lines" "makeArray" "manifestIni" "manifestPython" "manifestPythonVars" "mantissa" "map" "max" "md5" "mergePatch" "min" "mod" "objectFields" "objectFieldsAll" "objectHas" "objectHasAll" "parseInt" "pow" "prune" "range" "set" "setDiff" "setInter" "setUnion" "sin" "sort" "split" "splitLimit" "sqrt" "startsWith" "stringChars" "substr" "substr" "tan" "thisFile" "toString" "type" "uniq")))))
    (list
     `(,builtin-regex . font-lock-builtin-face)
     `(,constant-regex . font-lock-constant-face)
     `(,function-name-regex . (1 font-lock-function-name-face))
     `(,variable-name-regex . (1 font-lock-variable-name-face))
     '("[[:space:]].+:" . font-lock-keyword-face)
     '("\\([[:digit:]]+\\(?:\\.[[:digit:]]+\\)?\\)" . font-lock-constant-face)
     `(,standard-functions-regex . font-lock-function-name-face)
     ))
  "Minimal highlighting for ‘jsonnet-mode’.")

(defvar jsonnet-font-lock-keywords jsonnet-font-lock-keywords-1
  "Default highlighting expressions for jsonnet mode.")

;; Syntax table
(defconst jsonnet-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments. Jsonnet supports /* */ and // as comment delimiters
    (modify-syntax-entry ?/ ". 124" table)
    (modify-syntax-entry ?* ". 23b" table)
    ;; Additionally, Jsonnet supports # as a comment delimiter
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; ", ', and ||| are quotations in Jsonnet.
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?| "\"" table) ; This incidentally also causes triple-quoted strings to be
                                        ; correctly highlighted.
    ;; Our parenthesis, braces and brackets
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table)
  "Syntax table for `jsonnet-mode'.")

;; Indent rules
(defun jsonnet--debug-print (str)
  "Print out STR if ‘jsonnet-enable-debug-print’ is non-nil."
  (when jsonnet-enable-debug-print
    (message str)))

(defun jsonnet--find-current-block-comment ()
  "Return the position of the comment start if inside a block comment. Otherwise, return nil."
  (let* ((previous-comment-start (save-excursion (re-search-backward "\\/\\*" nil t)))
         (previous-comment-end (save-excursion (re-search-backward "\\*\\/" nil t)))
         (is-in-block-comment (and (integerp previous-comment-start)
                                   (or (not (integerp previous-comment-end))
                                       (> previous-comment-start previous-comment-end)))))
    (when is-in-block-comment previous-comment-start)))

(defun jsonnet--line-matches-regex-p (regex)
  "Return t if the current line matches REGEX."
  (save-excursion
    (beginning-of-line)
    (integerp (re-search-forward regex (line-beginning-position 2) t))))

(defun jsonnet--prev-line-ends-with-open-brace-or-open-bracket-or-colon-p ()
  "Return t if the previous line ends with { [ or :, otherwise return nil."
  (save-excursion
    (forward-line -1)
    (jsonnet--line-matches-regex-p "[:{[]\s*$")))

(defun jsonnet--curr-line-ends-with-close-brace-or-close-bracket-p ()
  "Return t if the current line ends with } or } followed by comma, ;, { or [.
Otherwise return nil."
  (jsonnet--line-matches-regex-p "[]}]\s*\\(,\\(\s*[{[]\\)?\\|;\\)?\s*$"))

(defun jsonnet--prev-line-ends-with-comma-without-colon-or-brace-p ()
  "Return t if the previous line ends with a comma and does not contain a colon."
  (save-excursion
    (forward-line -1)
    (and (jsonnet--line-matches-regex-p ",\s*$")
         (not (jsonnet--line-matches-regex-p ":"))
         (not (jsonnet--line-matches-regex-p "\\}")))))

(defun jsonnet--curr-line-inside-multiline-string-p ()
  "Return t if the beginning of the line is inside of a multiline string, otherwise return nil."
  (save-excursion
    (beginning-of-line)
    (let ((num-triple-pipe 0))
      (while (search-backward "|||" 0 t)
        (setq num-triple-pipe (+ num-triple-pipe 1)))
      (eq 1 (% num-triple-pipe 2)))))

(defun jsonnet--curr-line-has-multiline-string-p (opens-multiline-string)
  "Return t if the current line is part of a multiline string.
If OPENS-MULTILINE-STRING is not nil, return t if the current line begins outside a multiline
string and ends inside one, otherwise return nil. If OPEN-MULTILINE-STRING is nil, return t if
the current line begins inside a multiline string and ends outside one, otherwise return nil."
 (save-excursion
   (beginning-of-line)
   ;; The previous line opens a multiline string if it contains ||| and there are an odd number of
   ;; triple pipes before the current line.
   (when (jsonnet--line-matches-regex-p "|||")
     (forward-line)
     (eq (if opens-multiline-string t nil)
         (jsonnet--curr-line-inside-multiline-string-p)))))

(defun jsonnet--prev-line-opened-multiline-string-p ()
  "Return t if the previous line opened a multiline string, otherwise return nil."
  (save-excursion
    (forward-line -1)
    (jsonnet--curr-line-has-multiline-string-p t)))

(defun jsonnet--prev-line-closed-multiline-string-p ()
  "Return t if the previous line closed a multiline string, otherwise return nil."
  (save-excursion
    (forward-line -1)
    (jsonnet--curr-line-has-multiline-string-p nil)))

(defun jsonnet-curr-line-closed-multiline-string-p ()
  "Return t if the current line closed a multiline string, otherwise return nil."
  (jsonnet--curr-line-has-multiline-string-p nil))

(defun jsonnet-calculate-indent ()
  "Calculates the indent for the current line."
  (interactive)
  (save-excursion
      (beginning-of-line)
      ;; At the beginning of the file, the indentation should be 0.
      (if (bobp)
          0
        (let ((current-block-comment (jsonnet--find-current-block-comment))
              (previous-block-comment (save-excursion
                                        (beginning-of-line -1)
                                        (jsonnet--find-current-block-comment))))
          (cond
           ;; NOTE: In all of these examples, the 'o' indicates the location of point after
           ;; indenting on that line. If the indent of the line depends on the contents of the line
           ;; itself, a '^' is used to indicate the proper indentation for the last line.
           ;;
           ;; If we are in a block comment, the indent should match the * at the beginning of the
           ;; comment.
           ;; e.g.
           ;; |/*
           ;; | o
           (current-block-comment
            (jsonnet--debug-print "Current line is in a block comment")
            (goto-char current-block-comment)
            (+ 1 (current-column)))

           ;; If the previous line opened a multiline string, and the current line does not close a
           ;; multiline string, increase indentation.
           ;; e.g.
           ;; |  myField: |||
           ;; |    o
           ((and (jsonnet--prev-line-opened-multiline-string-p)
                 (not (jsonnet-curr-line-closed-multiline-string-p)))
            (jsonnet--debug-print "Previous line opened multiline string not closed on current line")
            (backward-to-indentation)
            (+ (current-column) tab-width))

           ;; If the current line closed a multiline string, decrease indentation.
           ;; e.g.
           ;; |  myField: |||
           ;; |    some text
           ;; |  |||
           ;;    ^ proper indentation.
           ((jsonnet-curr-line-closed-multiline-string-p)
            (jsonnet--debug-print "Current line closed multiline string")
            (backward-to-indentation)
            (- (current-column) tab-width))

           ;; If the current line is inside of a multiline string, do nothing.
           ;; e.g.
           ;; |  myField: |||
           ;; |    some text
           ;; |    o
           ((jsonnet--curr-line-inside-multiline-string-p)
            (backward-to-indentation)
            (current-column))

           ;; If the current line ends with a close brace and the previous line ends with a comma
           ;; without colon or brace, doubly de-indent.
           ;; e.g.
           ;; |  myField:
           ;; |    myValue,
           ;; |}
           ;;  ^ proper indentation.
           ((and (jsonnet--curr-line-ends-with-close-brace-or-close-bracket-p)
                 (jsonnet--prev-line-ends-with-comma-without-colon-or-brace-p))
            (jsonnet--debug-print "Current line ended with close brace and last line ended with comma without colon")
            (backward-to-indentation)
            (- (current-column) (* 2 tab-width)))

           ;; If the previous line ends with a : or {, or the line opens a multiline string,
           ;; increase indentation.
           ;; e.g.
           ;; |  myField:
           ;; |    o
           ;; or
           ;; |  myField: {
           ;; |    o
           ;; or
           ;; |  myField: [
           ;; |    o
           ((jsonnet--prev-line-ends-with-open-brace-or-open-bracket-or-colon-p)
            (jsonnet--debug-print "Previous line ended with open brace or open bracket or colon")
            (backward-to-indentation)
            (+ tab-width (current-column)))

           ;; If the current line ends with a }, decrease indentation.
           ;; e.g.
           ;; |  myField: {
           ;; |    innerField: innerValue,
           ;; |  },
           ;; |}
           ;;  ^ proper indentation
           ;; Note that the comma on the third line should not affect the indentation on the fourth
           ;; line.
           ((jsonnet--curr-line-ends-with-close-brace-or-close-bracket-p)
            (jsonnet--debug-print "Current line ended with close brace")
            (backward-to-indentation)
            (- (current-column) tab-width))

           ;; If the previous line is a comment, use the indent of the comment.
           ;; e.g.
           ;; |  myField: "myValue",
           ;; |  // a comment
           ;; |  o
           (previous-block-comment
            (jsonnet--debug-print "Previous line is block comment")
            (goto-char previous-block-comment)
            (jsonnet-calculate-indent))

           ;; Otherwise, the indent is unchanged.
           (t
            (jsonnet--debug-print "Indent is unchanged")
            (backward-to-indentation)
            (current-column)))))))

(defun jsonnet-indent ()
  "Indent current line according to Jsonnet syntax."
  (interactive)
  (let ((calculated-indent (jsonnet-calculate-indent)))
    (when (not (eq calculated-indent (current-indentation)))
      (beginning-of-line)
      (delete-char (current-indentation))
      (indent-to calculated-indent))))

;;;###autoload
(define-derived-mode jsonnet-mode prog-mode "Jsonnet"
  "jsonnet-mode is a major mode for editing .jsonnet files."
  :syntax-table jsonnet-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults) '(jsonnet-font-lock-keywords ;; keywords
                                                   nil                        ;; keywords-only
                                                   nil                        ;; case-fold
                                                   nil                        ;; syntax-alist
                                                   nil                        ;; syntax-begin
                                                   ))
  (set (make-local-variable 'indent-line-function) 'jsonnet-indent))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.jsonnet\\'" 'jsonnet-mode))

;; Utilities for evaluating and jumping around Jsonnet code.
;;;###autoload
(defun jsonnet-eval ()
  "Run jsonnet with the path of the current file."
  (interactive)
  (let ((buffer-to-eval (buffer-file-name)))
    (when (buffer-modified-p)
      (when (y-or-n-p
             (format "Save file %s? " buffer-to-eval))
        (save-buffer)))
    (with-current-buffer (get-buffer-create "*jsonnet output*")
      (erase-buffer)
      (call-process jsonnet-command nil t nil buffer-to-eval)
      (when (fboundp 'json-mode)
        (json-mode))
      (display-buffer (current-buffer)
                      '((display-buffer-pop-up-window
                         display-buffer-reuse-window
                         display-buffer-at-bottom
                         display-buffer-pop-up-frame))))))

(define-key jsonnet-mode-map (kbd "C-c C-c") 'jsonnet-eval)

;;;###autoload
(defun jsonnet-jump-to-definition (identifier)
  "Jump to the definition of the jsonnet function IDENTIFIER."
  (interactive "sFind definition with name: ")
  (let* ((local-def (concat "local\s+" identifier "[^[:alnum:]_]"))
         (inner-def (concat identifier "\\:+"))
         (full-regex (concat "\\(" local-def "\\|" inner-def "\\)"))
         (identifier-def (save-excursion
                         (goto-char (point-max))
                         (re-search-backward full-regex nil t))))
    (if identifier-def
        (goto-char identifier-def)
      (message (concat "Unable to find definition for " identifier ".")))))

(defun jsonnet--get-identifier-at-location (&optional location)
  "Return the identifier at LOCATION if over a Jsonnet identifier.
If not provided, current point is used."
  (save-excursion
    (when location
      (goto-char location))
    (let ((curr-point (point))
          (curr-char (char-after)))
      (when (or (eq ?_ curr-char)
                (<= ?a curr-char ?z)
                (<= ?A curr-char ?Z)
                (<= ?0 curr-char ?9))
        (let ((start (save-excursion
                       (skip-chars-backward "[:alnum:]_")
                       (skip-chars-forward "[:digit:]")
                       (point)))
              (end   (save-excursion
                       (skip-chars-forward "[:alnum:]_")
                       (point))))
          (when (<= start curr-point end)
            (buffer-substring start end)))))))

;;;###autoload
(defun jsonnet-jump (point)
  "Jumps to the definition of the Jsonnet expression at POINT."
  (interactive "d")
  (let ((current-identifier (jsonnet--get-identifier-at-location)))
    (if (not current-identifier)
        (message "Point is not over a valid Jsonnet identifier.")
      (jsonnet-jump-to-definition current-identifier))))

(define-key jsonnet-mode-map (kbd "C-c C-f") 'jsonnet-jump)

;;;###autoload
(defun jsonnet-reformat-buffer ()
  "Reformat entire buffer using the Jsonnet format utility."
  (interactive)
  (call-process-region (point-min) (point-max) jsonnet-command t t nil "fmt" "-"))

(define-key jsonnet-mode-map (kbd "C-c C-r") 'jsonnet-reformat-buffer)

(provide 'jsonnet-mode)
;;; jsonnet-mode.el ends here
