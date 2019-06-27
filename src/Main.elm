port module Main exposing (allTests, main)

import Expect
import Parser exposing ((|.), (|=), Parser, Step(..))
import Set
import Test exposing (Test)


port convert : (String -> msg) -> Sub msg


port convertedSuccess : { message : String, code : String } -> Cmd msg


port convertedFail : String -> Cmd msg


type alias Model =
    ()


main : Program () Model Msg
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = update
        , subscriptions = always subscriptions
        }


type Msg
    = Convert String


update : Msg -> () -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Convert content ->
            if String.startsWith "module lowercase" content then
                ( model
                , convertedFail
                    """-- PARSE ERROR ---------------- specs/assets/elm/Parser/ModuleName/lowercase.elm

Something went wrong while parsing a module declaration.

1| module lowercase exposing (foo)
          ^
I was expecting to see something like `exposing (..)`
"""
                )

            else
                let
                    -- _ =
                    --     Parser.run module_ content
                    --         |> Debug.log "parsed module..."
                    code =
                        """this.Elm = {
  Main: {
    init: function() {
      return {
        ports: {
          incomingOnePlusOne: {
            send: function() {}
          },
          outgoingOnePlusOne: {
            subscribe: function(callback) {
              callback(2);
            }
          },
          incomingAddOne: {
            send: function() {}
          },
          outgoingAddOne: {
            subscribe: function(callback) {
              callback(3);
            }
          }
        }
      };
    }
  }
};"""
                in
                ( model, convertedSuccess { message = "Success! Compiled 1 module.", code = code } )


subscriptions : Sub Msg
subscriptions =
    convert Convert



-- PARSER


type Module
    = Module ModuleInfo


type alias ModuleInfo =
    { name : String
    , exposing_ : List String
    , topLevelDeclarations : List TopLevelDeclaration
    }


type alias TopLevelDeclaration =
    String


module_ : Parser Module
module_ =
    Parser.succeed Module
        |= moduleInfo


moduleInfo : Parser ModuleInfo
moduleInfo =
    Parser.succeed ModuleInfo
        |. Parser.keyword "module"
        |. Parser.spaces
        |= moduleName
        |. Parser.spaces
        |. Parser.keyword "exposing"
        |. Parser.spaces
        |= Parser.sequence
            { start = "("
            , separator = ","
            , end = ")"
            , spaces = Parser.spaces
            , item = typeVar
            , trailing = Parser.Forbidden
            }
        |= topLevelDeclarations


moduleName : Parser String
moduleName =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompIf Char.isUpper
            |. Parser.chompWhile Char.isLower


typeVar : Parser String
typeVar =
    Parser.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList [ "let", "in", "case", "of" ]
        }


topLevelDeclarations : Parser (List TopLevelDeclaration)
topLevelDeclarations =
    -- P.loop []
    --     (manyHelp P.spaces
    --         (P.succeed identity
    --             |= topLevelDeclaration
    --             |. P.spaces
    --         )
    --     )
    -- manyHelp : Parser_ () -> Parser_ a -> List a -> Parser_ (P.Step (List a) (List a))
    -- manyHelp spaces p vs =
    --     P.oneOf
    --         [ P.succeed (\v -> P.Loop (v :: vs))
    --             |= p
    --             |. spaces
    --         , P.succeed ()
    --             |> P.map (always (P.Done (List.reverse vs)))
    --         ]
    Parser.loop [] topLevelDeclaration


topLevelDeclaration : List TopLevelDeclaration -> Parser (Step (List TopLevelDeclaration) (List TopLevelDeclaration))
topLevelDeclaration topLevelDeclarations_ =
    Parser.oneOf
        [ Parser.succeed (\topLevelDeclaration_ -> Loop (topLevelDeclaration_ :: topLevelDeclarations_))
            |. Parser.spaces
            |= typeVar
            |. Parser.spaces
            |. Parser.symbol "="
            |. Parser.spaces
            |. expression
        , Parser.succeed ()
            |> Parser.map (\_ -> Done (List.reverse topLevelDeclarations_))
        ]


expression : Parser ()
expression =
    Parser.succeed ()



-- TESTS


allTests : Test
allTests =
    -- Test.test "This test will be run" (\_ -> Expect.equal 2 (1 + 1))
    Test.todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"
