(describe "jsonnet-eval-buffer"
  (after-each (delete-other-windows))

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
    (expect '("// -*- jsonnet-command-options: (\"-V\" \"bar=world\"); -*-"
              "{"
              "  Hello: std.extVar('bar'),"
              "}")
            :to-render-as
            '("{"
              "   \"Hello\": \"world\""
              "}"))))
