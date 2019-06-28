port module Main exposing (allTests, main)

import Expect
import Parser.Advanced as Parser exposing ((|.), (|=), Parser, Step(..), Token)
import Set
import Test exposing (Test)



-- PORTS


port convert : (String -> msg) -> Sub msg


port convertedSuccess : { message : String, code : String } -> Cmd msg


port convertedFail : String -> Cmd msg



-- MAIN


main : Program () Model Msg
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = update
        , subscriptions = always subscriptions
        }



-- MODEL


type alias Model =
    ()



-- MSG


type Msg
    = Convert String



-- UPDATE


update : Msg -> () -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Convert content ->
            case Parser.run chompModule content of
                Ok _ ->
                    let
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

                Err _ ->
                    ( model
                    , convertedFail
                        """-- PARSE ERROR ---------------- specs/assets/elm/Parser/ModuleName/lowercase.elm

Something went wrong while parsing a module declaration.

1| module lowercase exposing (foo)
          ^
I was expecting to see something like `exposing (..)`
"""
                    )



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    convert Convert



-- PARSER


type alias ElmParser a =
    Parser Context Problem a


type Context
    = Definition String
    | List
    | Record


type Problem
    = ModuleName
    | FreshLine Row Col


type alias Row =
    Int


type alias Col =
    Int


type Module
    = Module
        -- { _header :: Maybe Header
        -- , _imports :: [Src.Import]
        -- , _infixes :: [A.Located Src.Infix]
        -- , _decls :: [Decl.Decl]
        -- }
        { header : Maybe Header
        , imports : List ()
        , infixes : List ()
        , decls : List ()
        }


type Header
    = Header
        -- Header (A.Located Name.Name) Effects (A.Located Src.Exposing) (Either A.Region Src.Comment)
        { name : ( String, List String )
        , effects : ()
        }


type alias Located a =
    { start : ( Row, Col )
    , value : a
    , end : ( Row, Col )
    }


located : ElmParser a -> ElmParser (Located a)
located parser =
    Parser.succeed Located
        |= Parser.getPosition
        |= parser
        |= Parser.getPosition


chompModule : ElmParser Module
chompModule =
    Parser.succeed
        (\header ->
            Module
                { header = header
                , imports = []
                , infixes = []
                , decls = []
                }
        )
        |= chompHeader


chompHeader : ElmParser (Maybe Header)
chompHeader =
    Parser.succeed
        (\start maybeName ->
            Maybe.map
                (\name ->
                    Header
                        { name = name
                        , effects = ()
                        }
                )
                maybeName
        )
        |. freshLine
        |= Parser.getPosition
        |= Parser.oneOf
            [ Parser.map Just moduleNameNoEffects
            , Parser.map Just moduleNamePorts
            , Parser.succeed Nothing
            ]


moduleNameNoEffects : ElmParser ( String, List String )
moduleNameNoEffects =
    Parser.succeed identity
        |. Parser.keyword moduleToken
        |. Parser.spaces
        |= moduleName


moduleNamePorts : ElmParser ( String, List String )
moduleNamePorts =
    Parser.succeed identity
        |. Parser.keyword moduleToken
        |. Parser.spaces
        |. Parser.keyword moduleToken
        |. Parser.spaces
        |= moduleName


moduleName : ElmParser ( String, List String )
moduleName =
    moduleNameWithoutDots
        |> Parser.andThen (\topModuleName -> Parser.loop ( topModuleName, [] ) moduleNameHelp)


moduleNameHelp : ( String, List String ) -> ElmParser (Step ( String, List String ) ( String, List String ))
moduleNameHelp ( topModuleName, revModuleNames ) =
    Parser.oneOf
        [ Parser.succeed (\name -> Loop ( topModuleName, name :: revModuleNames ))
            |. Parser.symbol (Parser.Token "." ModuleName)
            |= moduleNameWithoutDots
        , Parser.succeed ()
            |> Parser.map (\_ -> Done ( topModuleName, List.reverse revModuleNames ))
        ]


moduleNameWithoutDots : ElmParser String
moduleNameWithoutDots =
    Parser.variable
        { start = Char.isUpper
        , inner = Char.isAlphaNum
        , reserved = Set.empty
        , expecting = ModuleName
        }


moduleToken : Token Problem
moduleToken =
    Parser.Token "module" ModuleName


freshLine : ElmParser ()
freshLine =
    Parser.succeed ()
        |. Parser.spaces
        |. checkFreshLine


checkFreshLine : ElmParser ()
checkFreshLine =
    Parser.succeed identity
        |= Parser.getPosition
        |> Parser.andThen
            (\( row, col ) ->
                if col == 1 then
                    Parser.succeed ()

                else
                    Parser.problem (FreshLine row col)
            )



-- TESTS


allTests : Test
allTests =
    moduleNameTests


moduleNameTests : Test
moduleNameTests =
    Test.describe "moduleName"
        [ Test.test "Main" <|
            \_ ->
                Expect.equal (Parser.run moduleName "Main") (Ok ( "Main", [] ))
        , Test.test "lowercase" <|
            \_ ->
                Expect.equal (Parser.run moduleName "lowercase") (Err [ { col = 1, contextStack = [], problem = ModuleName, row = 1 } ])
        , Test.test "Nested.Module.Name" <|
            \_ ->
                Expect.equal (Parser.run moduleName "Nested.Module.Name") (Ok ( "Nested", [ "Module", "Name" ] ))
        ]
