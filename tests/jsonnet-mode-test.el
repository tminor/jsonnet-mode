;;; jsonnet-mode-test.el --- Jsonnet Mode Tests -*- lexical-binding: t -*-

(when (require 'undercover nil t)
  (let ((undercover-force-coverage t))
    (undercover "*.el" "tests/*.el"
                (:report-file "./coverage-final.json")
                (:send-report nil)
                (:report-format 'coveralls))))

(require 'jsonnet-mode)
(require 'undercover)

(progn (require 'f)
       (add-to-list 'load-path (f-parent (f-parent (f-this-file))))
       (require 'jsonnet-test))
