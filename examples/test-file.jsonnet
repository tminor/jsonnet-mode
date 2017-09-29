/*
 * This is a comment.
 */
local myOtherFile = import "other-test-file.jsonnet";

// This is a comment
local testFunction(paramA, paramB) = paramA + paramB;

local testFunction2(x) = if x > 3 then "test" else null;

local testFunction3 = function(a, b, c) {
  dd: "blerp",
  test3:
    "my value",
};

local testIdentifierWithNoNumbers = null;
local testIdentifierWith1Number = null;
local test_identifier_with_underscores = null;
local test_identifier_with_underscores_and_1_number = null;

{
  /*
   *
   */
  myVisibleOutput: testFunction("test1", "test2"),
  myHiddenOutput:: "hidden" + $.wubba,
  myField1:
    "myValue1",
  mySubObject: {
    a: 3,
    b: 45,
  },
  myMultilineString: |||
    It's a string.
  |||,
}
