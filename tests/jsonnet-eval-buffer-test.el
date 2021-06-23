(describe "jsonnet-eval-buffer"
  (after-all (kill-buffer "*jsonnet output*"))

  (it "compiles a valid Jsonnet program and renders the result"
    (expect '("{"
              "  foo: 'bar',"
              "}"
              "")
            :to-render-as
            '("{"
              "   \"foo\": \"bar\""
              "}")))

  (it (concat "compiles and renders JSON when provided a valid Jsonnet program "
              "containing a magic comment specifying a value for jsonnet-command-options")
    (let ((jsonnet-command-options '("-V" "foo=Hello")))
      (expect '("// -*- jsonnet-command-options: '(\"-V\" \"bar=world\"); -*-"
                "{"
                "  [std.extVar('foo')]: std.extVar('bar'),"
                "}")
              :to-render-as
              '("{"
                "   \"Hello\": \"world\""
                "}")))))
