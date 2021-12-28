(describe "The indentation function"

  (describe "inside function parameters/arguments"
    (it "should indent function bodies"
      (expect '("std.foldl("
                "  function(x, y)"
                "    x + y,"
                "  [1, 2, 3],"
                "  0"
                ")")
              :indented))
    (it "should indent anonymous function parameters"
      (expect '("function("
                "  x,"
                "  y"
                ")"
                "  x + y")
              :indented))
    (it "should indent named function args"
      (expect '("std.format("
                "  \"%s\","
                "  \"foo\""
                ")")
              :indented)))

  (describe "inside objects"
    (it "should indent field names and values"
      (expect '("{"
                "  foo:"
                "    1,"
                "  bar: 2"
                "}")
              :indented))
    (it "should indent local variables"
      (expect '("{"
                "  local foo = {"
                "    f: 1"
                "  }"
                "}")
              :indented))
    (it "should indent single line function bodies"
      (expect '("{"
                "  foo(x)::"
                "    x"
                "}")
              :indented))
    (it "should indent multiline function bodies"
      (expect '("{"
                "  foo(x)::"
                "    x +"
                "    5"
                "}")
              :indented))
    (it "should indent function args"
      (expect '("{"
                "  foo("
                "    x,"
                "    y"
                "  )::"
                "    x + y"
                "}")
              :indented))
    (it "should indent nested objects"
      (expect '("{"
                "  foo: {"
                "    bar: 1"
                "  }"
                "}")
              :indented))
    (it "should indent nested objects on a new line"
      (expect '("{"
                "  foo:"
                "    {"
                "      bar: 1"
                "    }"
                "}")
              :indented))
    (it "should indent boolean logic"
      (expect '("{"
                "  foo:"
                "    if true then"
                "      1"
                "    else"
                "      2"
                "}")
              :indented))
    (it "should indent fields and values separated by a blank line"
      (expect '("{"
                "  foo:"
                "\n"
                "    1"
                "}")
              :indented)))

  (describe "inside arrays"
    (it "should indent elements separated by a new line"
      (expect '("foo = ["
                "  \"bar\""
                "]")
              :indented)))

  (describe "inside comprehensions"
    (it "should indent an array comprehension body"
      (expect '("["
                "  foo"
                "  for foo in ['bar', 'baz', 'boo']"
                "]")
              :indented))
    (it "should indent an object comprehension body"
      (expect '("{"
                "  [foo]: 'bar'"
                "  for foo in ['bar', 'baz', 'boo']"
                "}")
              :indented)))

  (describe "at end of file"
    (it "should not indent comments"
      (expect '("{}"
                ""
                "// END")
              :indented))))
