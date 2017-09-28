
;;; jsonnet-mode.el -- Major mode for editing jsonnet files

;; Copyright (C) 2017 Nick Lanham

;; Author: Nick Lanham
;; URL: https://github.com/mgyucht/jsonnet-mode
;; Version: 0.1

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

;; This package provides simple syntax highlighting for jsonnet files.
;; To use it, place it somewhere in your load-path, and then add the
;; following to your init.el:
;; (load "jsonnet-mode")
;; To autoload for .jsonnet files add:
;; (add-to-list 'auto-mode-alist '("\\.jsonnet\\'" . jsonnet-mode))
;;
;; See jsonnet-flycheck.el for information on how to enable flychecking.
;;
;; This mode binds two keys:
;;   'C-c e' evaluates the current buffer in jsonnet and put the output in an output buffer
;;   'M-.' Checks if the point (cursor) is inside an import string (like: import "foo.jsonnet")
;;         and jumps to that file if it is.  You must be inside the actual string for this to work.

;;; Code:

(defcustom jsonnet-command
  "jsonnet"
  "Jsonnet command to run in eval-jsonnet, and for the flychecker."
  :type '(string)
  :group 'jsonnet)

(defvar jsonnet-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `jsonnet-mode'.")

;; (defun jsonnet-find-multiline-string (limit)
;;   "Search for multiline string up to LIMIT."
;;   (interactive)
;;   (message (format "starting at: %i" (point)))
;;   (when (re-search-forward "|||" limit t)
;;     ;; Save the start position
;;     (let ((start (match-beginning 0)))
;;       ;; Go to the last |.
;;       (message (format "Start is: %i" start))
;;       (goto-char (match-end 0))
;;       (message (format "aftermove: %i" (point)))
;;       ;; Look for the next |||
;;       (let ((end (re-search-forward "|||" limit 'mv)))
;;         (message (format "end: %i" end))
;;         (message (format "md: %s" (match-data 't)))
;;         (if end
;;             (progn
;;               (set-match-data (list start end (buffer-name)))
;;               (message (format "md2: %s" (match-data 't)))
;;               end)
;;           (jsonnet-find-multiline-string limit))))))

(defconst jsonnet-font-lock-keywords-1
  (list
   '("\\<\\(assert\\|e\\(?:lse\\|rror\\)\\|f\\(?:or\\|unction\\)\\|i\\(?:mport\\(?:str\\)?\\|[fn]\\)\\|local\\|s\\(?:elf\\|uper\\)\\|then\\)\\>" . font-lock-builtin-face)
   '("\\<\\(false\\|null\\|true\\)\\>" . font-lock-constant-face)
   '("[[:space:]].+:" . font-lock-keyword-face)
   '("function \\(\\sw+\\)" . font-lock-function-name-face)
   '("\\([[:digit:]]+\\(?:\\.[[:digit:]]+\\)?\\)" . font-lock-constant-face)
   ;;'("\\(|||\\(.\\|\n\\)*?|||\\)" . font-lock-string-face)
   ;;'(jsonnet-find-multiline-string . font-lock-string-face)
   '("\\(?:std\\.\\(?:assertEqual\\|base64\\(?:Decode\\(?:Bytes\\)?\\)?\\|c\\(?:har\\|o\\(?:\\(?:depoi\\|u\\)nt\\)\\)\\|e\\(?:ndsWith\\|scapeString\\(?:Bash\\|Dollars\\|\\(?:Js\\|Pyth\\)on\\)\\|xtVar\\)\\|f\\(?:ilter\\(?:Map\\)?\\|lattenArrays\\|o\\(?:ld[lr]\\|rmat\\)\\)\\|join\\|l\\(?:ength\\|ines\\)\\|m\\(?:a\\(?:keArray\\|nifest\\(?:Ini\\|Python\\(?:Vars\\)?\\)\\|p\\)\\|d5\\|ergePatch\\)\\|object\\(?:Fields\\(?:All\\)?\\|Has\\(?:All\\)?\\)\\|parseInt\\|range\\|s\\(?:et\\(?:Diff\\|Inter\\|Union\\)?\\|ort\\|plit\\(?:Limit\\)?\\|t\\(?:artsWith\\|ringChars\\)\\|ubstr\\)\\|t\\(?:hisFile\\|oString\\|ype\\)\\|uniq\\)\\)" . font-lock-function-name-face)
   )
  "Minimal highlighting for jsonnet-mode.")

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
    (modify-syntax-entry ?| "\"" table)
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
(defun find-current-block-comment ()
  "Returns the position of the comment start if the point is inside of a block comment. Otherwise,
returns nil."
  (let* ((previous-comment-start (save-excursion (re-search-backward "\\/\\*" nil t)))
         (previous-comment-end (save-excursion (re-search-backward "\\*\\/" nil t)))
         (is-in-block-comment (and (integerp previous-comment-start)
                                   (or (not (integerp previous-comment-end))
                                       (> previous-comment-start previous-comment-end)))))
    (when is-in-block-comment previous-comment-start)))

(defun line-matches-regex-p (regex)
  "Returns t if the current line matches the provided regular expression."
  (save-excursion
    (beginning-of-line)
    (integerp (re-search-forward regex (line-beginning-position 2) t))))

(defun prev-line-ends-with-open-brace-or-colon-p ()
  "Returns t if the previous line ends with { or :, otherwise returns nil."
  (save-excursion
    (forward-line -1)
    (line-matches-regex-p "[:{]\s*$")))

(defun curr-line-ends-with-close-brace-p ()
  "Returns t if the current line ends with }, otherwise returns nil."
  (line-matches-regex-p "\\}\s*$"))

(defun prev-line-ends-with-comma-without-colon-p ()
  "Returns t if the previous line ends with a comma and does not contain a colon."
  (save-excursion
    (forward-line -1)
    (and (line-matches-regex-p ",\s*$")
         (not (line-matches-regex-p ":")))))

(defun jsonnet-calculate-indent ()
  "Calculates the indent for the current line."
  (interactive)
  (save-excursion
      (beginning-of-line)
      (if (bobp)
          0
        (let ((current-block-comment (find-current-block-comment))
              (previous-block-comment (save-excursion
                                        (beginning-of-line -1)
                                        (find-current-block-comment))))
          (cond
           ;; If we are in a block comment, the indent should match the * at the beginning of the comment.
           (current-block-comment
            (print "Current line is in a block comment")
            (goto-char current-block-comment)
            (+ 1 (current-column)))

           ;; If the previous line ends with a : or {, increase indentation.
           ((prev-line-ends-with-open-brace-or-colon-p)
            (print "Previous line ended with open brace or colon")
            (backward-to-indentation)
            (+ tab-width (current-column)))

           ((and (curr-line-ends-with-close-brace-p)
                 (prev-line-ends-with-comma-without-colon-p))
            (print "Current line ended with close brace and last line ended with comma without colon")
            (backward-to-indentation)
            (- (current-column) (* 2 tab-width)))

           ;; If the current line ends with a }, decrease indentation.
           ;; If the previous line ends with a comma and does not have a :, decrease indentation.
           ((or (curr-line-ends-with-close-brace-p)
                (prev-line-ends-with-comma-without-colon-p))
            (print "Current line ended with close brace or last line ended with comma without colon")
            (backward-to-indentation)
            (- (current-column) tab-width))

           ;; If the previous line is a comment, skip backwards and recalculate the indent.
           (previous-block-comment
            (print "Previous line is block comment")
            (goto-char previous-block-comment)
            (jsonnet-calculate-indent))

           ;; Otherwise, the indent is unchanged.
           (t
            (print "Indent is unchanged")
            (backward-to-indentation)
            (current-column))
           )))))

(defun jsonnet-indent ()
  "Indent current line according to Jsonnet syntax.
The rules for Jsonnet indenting are as follows:
1. At the beginning of the buffer, indent is 0.
2. If the point is inside of a /* comment not started on this line, the indent is the same as that of the * in the /*.
4. If the preceding line ends with { or :, the indent should increase by tab-width.
5. If the current line ends with }, the indent should descrease by tab-width.
6. If the preceding line ends with a comma and does not include a :, the indent should decrease by tab-width.
7. Otherwise, the indent is the same as the preceding line."
  (interactive)
  (let ((calculated-indent (jsonnet-calculate-indent)))
    (when calculated-indent
      (back-to-indentation)
      (delete-char (current-indentation))
      (indent-to calculated-indent))))

;;;###autoload
(define-derived-mode jsonnet-mode prog-mode "Jsonnet"
  "jsonnet-mode is a major mode for editing .jsonnet files."
  :syntax-table jsonnet-mode-syntax-table
  (setq-local font-lock-defaults '(jsonnet-font-lock-keywords ;; keywords
                                   nil                        ;; keywords-only
                                   nil                        ;; case-fold
                                   nil                        ;; syntax-alist
                                   nil                        ;; syntax-begin
                                   ))
  (setq-local indent-line-function 'jsonnet-indent))

(defun jsonnet-eval ()
  "Run jsonnet with the path of the current file."
  (interactive)
  (when (buffer-modified-p)
    (when (y-or-n-p
           (format "Save file %s? " buffer-file-name))
      (save-buffer)))
  (progn
    (when (get-buffer "*jsonnet output*")
      (kill-buffer "*jsonnet output*"))
    (call-process jsonnet-command nil "*jsonnet output*" nil (buffer-file-name))
    (set-buffer "*jsonnet output*")
    (when (fboundp 'json-mode)
      (json-mode))
    (display-buffer
     (get-buffer "*jsonnet output*")
     '((display-buffer-reuse-window
        display-buffer-pop-up-window
        display-buffer-pop-up-frame)) )))

(defun is-import-str (start)
  "Return non-nil if, from START we find 'import '."
  (save-excursion
    (goto-char start)
    (re-search-forward "import " (+ start 7) t)))

(defun jsonnet-visit-file ()
  "If the point is on a jsonnet 'import' line, jump to that file."
  (interactive)
  (let ((parse (syntax-ppss (point))))
    (if (nth 3 parse)
        (if (is-import-str (nth 2 parse))
            (let* ((end (save-excursion (re-search-forward "\"")))
                   (importfile (buffer-substring (+ (nth 8 parse) 1) (- end 1))))
              (find-file importfile)) ))))

;; (defun find-jsonnet-function-with-name (func-name)
;;   "Jumps to the definition of the jsonnet function with the provided name."
;;   (let* ((local-func-def1 (concat "local\s+" func-name "\s*=\s*function"))
;;          (local-func-def2 (concat "local\s+" func-name "\\(.*\\)\s*="))
;;          (inner-func-def (concat func-name "\\:+"))
;;          (full-regex (regexp-opt (list local-func-def1
;;                                        local-func-def2
;;                                        inner-func-def))))
;;     (unless (re-search-backward local-func-def2 1 t)
;;       (message (concat "Unable to find definition for " func-name ".")))))


(define-key jsonnet-mode-map (kbd "M-.") 'jsonnet-visit-file)
(define-key jsonnet-mode-map (kbd "C-c e") 'jsonnet-eval)

(provide 'jsonnet-mode)
;;; jsonnet-mode.el ends here
