;;; jsonnet-test.el --- Testing Macros -*- lexical-binding: t -*-

;; Copyright © 2020 Thomas Minor <minort@gmail.com>
;;
;; jsonnet-mode is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; jsonnet-mode is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with jsonnet-mode.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Code to support buttercup-based tests.

;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'faceup)

;; TODO(tminor): Remove the following line after SMIE becomes default.
(setq jsonnet-use-smie t)

(defun jsonnet-trim-indent (text)
  "Remove indentation from TEXT."
  (->> text s-lines (-map #'s-trim-left) (s-join "\n")))

(defun jsonnet-buffer-string ()
  "Return buffer as text with beginning and ending empty space trimmed."
  (s-trim (buffer-substring-no-properties (point-min) (point-max))))

(buttercup-define-matcher :indented (text)
  (let* ((text (s-join "\n" (funcall text)))
         (text-no-indent (jsonnet-trim-indent text))
         (inhibit-message t))
    (jsonnet-mode)
    (insert text-no-indent)
    (indent-region (point-min) (point-max))
    (let ((text-with-indent (jsonnet-buffer-string))
          (inhibit-message nil))
      (delete-region (point-min) (point-max))
      (if (string= text text-with-indent)
          t
        `(nil . ,(format "\nGiven indented text \n\n%s\n\nwas instead indented to \n\n%s\n\n"
                         text text-with-indent))))))

(buttercup-define-matcher :to-render-as (text expect)
  (let* ((text (s-join "\n" (funcall text)))
         (expect (s-join "\n" (funcall expect)))
         (inhibit-message t)
         (tmp-file (make-temp-file "jsonnet-eval-test-"))
         (kill-buffer-query-functions '(t))
         (compilation-ask-about-save nil)
         (enable-local-variables :all)
         rendered-text)
    (with-current-buffer (or (find-buffer-visiting tmp-file)
                             (progn
                               (find-file tmp-file)
                               (find-buffer-visiting tmp-file)))
      (jsonnet-mode)
      (insert text)
      (hack-local-variables)
      (save-buffer)
      (jsonnet-eval-buffer)
      ;; `jsonnet-eval-buffer' should create its output buffer and open
      ;; it in a new window; calling `other-window' here should switch
      ;; to that window.
      (other-window 1)
      (setq rendered-text (jsonnet-buffer-string))
      (if (string= rendered-text expect)
          t
        `(nil . ,(format (concat "\nGiven Jsonnet program\n\n%s\n\n"
                                 "rendered JSON output\n\n%s\n\n"
                                 "but it should have been\n\n%s\n\n")
                         text
                         rendered-text
                         expect))))))

(defun jsonnet-test--format-faceup-results (faceup-results source-beg)
  "Format FACEUP-RESULTS using the correct source line indicated by SOURCE-BEG."
  (mapconcat (lambda (line)
               (let ((line-num (+ source-beg (nth 1 line)))
                     (want (car (nth 2 line)))
                     (got (car (nth 3 line))))
                 (concat "      Line " (int-to-string line-num) ":\n"
                         "        +" want "\n"
                         "        -" got)))
             faceup-results "\n"))

(buttercup-define-matcher :to-produce-faceup-markup (source-lines faceup-markup source-beg)
  (let* ((source (s-join "\n" (funcall source-lines)))
         (faceup-results (with-temp-buffer
                           (insert source)
                           (jsonnet-mode)
                           (font-lock-fontify-buffer)
                           (faceup-markup-buffer)))
         (faceup-test-explain t)
         (results (faceup-test-equal (s-join "\n" (funcall faceup-markup))
                                     faceup-results)))
    (if (listp results)
        `(nil . ,(concat "\n    Incorrect faceup at the following locations in tests/jsonnet-font-lock-test.el (+want -got):\n"
                         (jsonnet-test--format-faceup-results results (funcall source-beg))))
      t)))

(provide 'jsonnet-test)
;;; jsonnet-test.el ends here
