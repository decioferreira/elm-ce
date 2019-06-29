const elmMake = require("../helpers/elm-make");

describe("Module header", () => {
  test("Main (Success! Compiled 1 module.)", done => {
    elmMake("Parser/ModuleHeader/Main.elm")
      .success(1)
      .end(done);
  });

  test("NoEffects", done => {
    elmMake("Parser/ModuleHeader/NoEffects.elm")
      .success()
      .end(done);
  });

  test("Ports", done => {
    elmMake("Parser/ModuleHeader/Ports.elm")
      .success()
      .end(done);
  });

  test("NoHeader", done => {
    elmMake("Parser/ModuleHeader/NoEffects.elm")
      .success()
      .end(done);
  });

  describe("Module name", () => {
    test("lowercaseName", done => {
      elmMake("Parser/ModuleHeader/lowercaseName.elm")
        .fail(
          `-- PARSE ERROR ---------- specs/assets/elm/Parser/ModuleHeader/lowercaseName.elm

Something went wrong while parsing a module declaration.

1| module lowercaseName exposing (foo)
          ^
I was expecting to see something like \`exposing (..)\`

`
        )
        .end(done);
    });

    test("Nested.Module.Name", done => {
      elmMake("Parser/ModuleHeader/Nested/Module/Name.elm")
        .success()
        .end(done);
    });
  });
});
