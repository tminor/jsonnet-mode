;;; jsonnet-flycheck.el --- add flychecking for jsonnet

;; Copyright (C) 2017 Nick Lanham

;; Author: Nick Lanham
;; URL: https://github.com/mgyucht/jsonnet-mode
;; Version: 0.0.1
;; Keywords: languages

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
;;
;; Provides flychecking for jsonnet.
;;
;; To use (assuming jsonnet-mode.el and jsonnet-flycheck.el are in the same dir):
;;  1. Install jsonnet-mode (see jsonnet-mode.el for instructions)
;;  2. Add: (load "jsonnet-flycheck") to your init.el
;;  3. That's it, this mode will activate anytime the buffer is in jsonnet-mode
;;
;; You can customize the jsonnet-command variable to point this
;; checker at an executable if the "jsonnet" command isn't in your
;; PATH.

;;; Code:

(require 'flycheck)
(require 'jsonnet-mode)

(flycheck-define-checker jsonnet
  "A jsonnet syntax checker."
  :command ("jsonnet"
            source-inplace)
  :error-patterns
    ((error line-start "STATIC ERROR: " (file-name) ":" line ":" column (zero-or-one (group "-" (one-or-more digit))) ": " (message) line-end)
     (error line-start "RUNTIME ERROR: " (message) "\n" (one-or-more space) (file-name) ":" (zero-or-one "(") line ":" column (zero-or-more not-newline) line-end))
  :modes jsonnet-mode
  :predicate (lambda () (buffer-file-name)))

(provide 'jsonnet-flycheck)
;;; jsonnet-flycheck.el ends here
