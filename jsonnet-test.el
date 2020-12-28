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
    (insert text-no-indent)
    (indent-region (point-min) (point-max))
    (let ((text-with-indent (jsonnet-buffer-string))
          (inhibit-message nil))
      (delete-region (point-min) (point-max))
      (if (string= text text-with-indent)
          t
        `(nil . ,(format "\nGiven indented text \n\n%s\n\nwas instead indented to \n\n%s\n\n"
                         text text-with-indent))))))

(provide 'jsonnet-test)
;;; jsonnet-test.el ends here
