const elmMake = require("../helpers/elm-make");

describe("Module name", () => {
  test("Main", done => {
    elmMake("Parser/ModuleName/Main.elm")
      .success(1)
      .end(done);
  });

  test("lowercase", done => {
    elmMake("Parser/ModuleName/lowercase.elm")
      .fail(
        `-- PARSE ERROR ---------------- specs/assets/elm/Parser/ModuleName/lowercase.elm

Something went wrong while parsing a module declaration.

1| module lowercase exposing (foo)
          ^
I was expecting to see something like \`exposing (..)\`

`
      )
      .end(done);
  });
});
