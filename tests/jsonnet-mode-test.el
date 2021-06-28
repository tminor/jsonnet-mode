;;; jsonnet-mode-test.el --- Jsonnet Mode Tests -*- lexical-binding: t -*-

(when (require 'undercover nil t)
  (let ((undercover-force-coverage t))
    (undercover "jsonnet-mode.el" "tests/*.el"
                (:send-report nil)
                (:report-format 'lcov)
                (:merge-report nil))))

(require 'jsonnet-mode)
(require 'undercover)

(progn (require 'f)
       (add-to-list 'load-path (f-parent (f-parent (f-this-file))))
       (require 'jsonnet-test))
