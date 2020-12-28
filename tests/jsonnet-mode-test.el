;;; jsonnet-mode-test.el --- Jsonnet Mode Tests -*- lexical-binding: t -*-

(when (require 'undercover nil t)
  (undercover "*.el"))

(require 'jsonnet-mode)

(progn (require 'f)
       (add-to-list 'load-path (f-parent (f-parent (f-this-file))))
       (require 'jsonnet-test))
