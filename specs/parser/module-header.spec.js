const elmMake = require("../helpers/elm-make");

describe("Module header", () => {
  test("Main (Success! Compiled 1 module.)", done => {
    elmMake("Parser/ModuleHeader/Main.elm")
      .success(1)
      .end(done);
  });

  test("NoEffects", done => {
    elmMake("Parser/ModuleHeader/NoEffects.elm")
      .success(1)
      .end(done);
  });

  test("Ports", done => {
    elmMake("Parser/ModuleHeader/Ports.elm")
      .success(1)
      .end(done);
  });

  test("NoHeader", done => {
    elmMake("Parser/ModuleHeader/NoHeader.elm")
      .success(1)
      .end(done);
  });

  describe("Module name", () => {
    test("lowercaseName", done => {
      elmMake("Parser/ModuleHeader/lowercaseName.elm")
        .fail(1, `-- EXPECTING MODULE NAME - specs/assets/elm/Parser/ModuleHeader/lowercaseName.elm

I was parsing an \`module\` declaration until I got stuck here:

1| module lowercaseName exposing (foo)
          ^
I was expecting to see the module name next, like in these examples:

    module Dict exposing (..)
    module Maybe exposing (..)
    module Html.Attributes exposing (..)
    module Json.Decode exposing (..)

Notice that the module names all start with capital letters. That is required!\n\n\n`
        )
        .end(done);
    });

    test("Nested.Module.Name", done => {
      elmMake("Parser/ModuleHeader/Nested/Module/Name.elm")
        .success(1)
        .end(done);
    });
  });
});
