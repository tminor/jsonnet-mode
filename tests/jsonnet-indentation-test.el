(require 'dash)
(require 's)

(defun jsonnet-trim-indent (text)
  "Remove indentation from TEXT."
  (->> text s-lines (-map #'s-trim-left) (s-join "\n")))

(defun jsonnet-buffer-string ()
  "Return buffer as text with beginning and ending empty space trimmed."
  (s-trim (buffer-substring-no-properties (point-min) (point-max))))

(buttercup-define-matcher :indented (text)
  (let* ((text (s-join "\n" (funcall text)))
         (text-no-indent (jsonnet-trim-indent text)))
    (insert text-no-indent)
    (indent-region (point-min) (point-max))
    (let ((text-with-indent (jsonnet-buffer-string)))
      (delete-region (point-min) (point-max))
      (if (string= text text-with-indent)
          t
        `(nil . ,(format "\nGiven indented text \n%s\nwas instead indented to \n%s\n"
                         text text-with-indent))))))

(describe "Indentation"
  (before-all (jsonnet-mode))

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
                "    1"
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
    (it "should indent fields and values separated by a blank new line"
      (expect '("{"
                "  foo:"
                " "
                "    1"
                "}")
              :indented))))
