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
         (text-no-indent (jsonnet-trim-indent text))
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
      (insert text-no-indent)
      (hack-local-variables)
      (save-buffer)
      (jsonnet-eval-buffer)
      (kill-buffer))
    (with-current-buffer "*jsonnet output*"
      (setq rendered-text (jsonnet-buffer-string)))
    (if (string= rendered-text expect)
        t
      `(nil . ,(format (concat "\nGiven Jsonnet program\n\n%s\n\n"
                               "rendered JSON output\n\n%s\n\n"
                               "but it should have been\n\n%s\n\n")
                       text
                       rendered-text
                       expect)))))

(provide 'jsonnet-test)
;;; jsonnet-test.el ends here
