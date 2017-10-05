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

local testFunctionWithManyArguments = function(
    a=null,
    b=1,
    c="test",
) = {
  a: a,
  b: b,
  c: c,
};

local testIdentifierWithNoNumbers = null;
local testIdentifierWith1Number = null;
local test_identifier_with_underscores = null;
local test_identifier_with_underscores_and_1_number = null;

{
  /*
   * This is a comment
   */
  myVisibleOutput: testFunction("test1", "test2"),
  myHiddenOutput:: "hidden" + $.wubba,
  myField1:
    "myValue",
  mySubObject: {
    a: 6,
    b: 45,
  },
  myArray: [
    "a thing",
    "another thing",
    {
      key1: "value1",
    }, {
      key2: "value2",
    },
  ],
  myMultilineString: |||
    It's a string.
    With more stuff inside of it!.
  |||,
}
