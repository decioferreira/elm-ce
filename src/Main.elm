port module Main exposing (allTests, main)

import Parser exposing ((|.), (|=), Parser)
import Test exposing (Test)


port convert : (String -> msg) -> Sub msg


port converted : String -> Cmd msg


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
            let
                _ =
                    Parser.run module_ content
                        |> Debug.log "parsed module..."

                js =
                    """
                    this.onePlusOne = 2;
                    this.addOne = function(a) { return 1 + a; };
                    """
            in
            ( model, converted js )


subscriptions : Sub Msg
subscriptions =
    convert Convert



-- PARSER


type Module
    = Module ModuleInfo


type alias ModuleInfo =
    { name : String
    , exposing_ : List String
    }


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
            , item = elmVar
            , trailing = Parser.Forbidden
            }
        |. Parser.spaces


moduleName : Parser String
moduleName =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompIf Char.isUpper
            |. Parser.chompWhile Char.isLower


elmVar : Parser String
elmVar =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompIf Char.isLower
            |. Parser.chompWhile (\c -> Char.isAlphaNum c || c == '_')



-- TESTS


allTests : Test
allTests =
    Test.todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"
