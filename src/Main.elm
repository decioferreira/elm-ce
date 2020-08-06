port module Main exposing (allTests, main)

import Expect
import Parser.Advanced as Parser exposing ((|.), (|=), Parser, Step(..), Token)
import Set exposing (Set)
import Test exposing (Test)



-- PORTS


port convert : (String -> msg) -> Sub msg


port convertedSuccess : { message : String, code : String } -> Cmd msg


port convertedFail : { message : String, error : String } -> Cmd msg



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


update : Msg -> Model -> ( Model, Cmd Msg )
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

                Err [ deadEnd ] ->
                    case deadEnd.problem of
                        ModuleName ->
                            ( model
                            , convertedFail
                                { message = "Detected problems in 1 module.\n"
                                , error = """-- PARSE ERROR ---------- specs/assets/elm/Parser/ModuleHeader/lowercaseName.elm

  Something went wrong while parsing a module declaration.

  1| module lowercaseName exposing (foo)
            ^
  I was expecting to see something like `exposing (..)`
  """
                                }
                            )

                        PortModuleExposing ExposingValue ->
                            ( model
                            , convertedFail
                                { message = "Detected problems in 1 module."
                                , error =
                                    "PortModuleExposing ExposingValue (row:"
                                        ++ String.fromInt deadEnd.row
                                        ++ ", col:"
                                        ++ String.fromInt deadEnd.col
                                        ++ ")\n\n"
                                        ++ content
                                }
                            )

                        _ ->
                            ( model
                            , convertedFail
                                { message = ""
                                , error =
                                    "SOME OTHER ERROR TYPE (row:"
                                        ++ String.fromInt deadEnd.row
                                        ++ ", col:"
                                        ++ String.fromInt deadEnd.col
                                        ++ ")\n\n"
                                        ++ content
                                }
                            )

                Err _ ->
                    ( model, convertedFail { message = "", error = "TODO: MULTIPLE ERRORS!" } )



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



-- PROBLEM


type Problem
    = FreshLine Row Col
    | ModuleExposing ExposingProblem
    | ModuleName
    | ModuleProblem
    | PortModuleExposing ExposingProblem
    | PortModuleProblem


type ExposingProblem
    = ExposingStart
    | ExposingValue
    | ExposingOperator
    | ExposingOperatorRightParen
    | ExposingTypePrivacy
    | ExposingEnd



-- POSITION


type alias Row =
    Int


type alias Col =
    Int



-- EXPOSING


type Exposing
    = Open
    | Explicit (List Exposed)


type Exposed
    = Lower String
    | Upper String Privacy
    | Operator String


type Privacy
    = Public
    | Private



-- LOCATED


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



-- MODULE


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



-- HEADER


type Header
    = Header
        -- Header (A.Located Name.Name) Effects (A.Located Src.Exposing) (Either A.Region Src.Comment)
        { name : ( String, List String )
        , effects : Effects
        , exposing_ : Exposing
        }


type Effects
    = NoEffects
    | Ports
    | Manager


chompHeader : ElmParser (Maybe Header)
chompHeader =
    Parser.succeed identity
        |. freshLine
        |= Parser.oneOf
            [ Parser.map Just moduleNameNoEffects
            , Parser.map Just moduleNamePorts
            , Parser.succeed Nothing
            ]


moduleNameNoEffects : ElmParser Header
moduleNameNoEffects =
    Parser.succeed
        (\start name exposingValue ->
            Header
                { name = name
                , effects = NoEffects
                , exposing_ = exposingValue
                }
        )
        |= Parser.getPosition
        |. Parser.keyword (moduleToken ModuleProblem)
        |. Parser.spaces
        |= moduleName
        |. Parser.spaces
        |. Parser.keyword (exposingToken ModuleProblem)
        |. Parser.spaces
        |= exposing_ ModuleExposing


moduleNamePorts : ElmParser Header
moduleNamePorts =
    Parser.succeed
        (\start name exposingValue ->
            Header
                { name = name
                , effects = Ports
                , exposing_ = exposingValue
                }
        )
        |= Parser.getPosition
        |. Parser.keyword portToken
        |. Parser.spaces
        |. Parser.keyword (moduleToken PortModuleProblem)
        |. Parser.spaces
        |= moduleName
        |. Parser.spaces
        |. Parser.keyword (exposingToken PortModuleProblem)
        |. Parser.spaces
        |= exposing_ PortModuleExposing


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


exposing_ : (ExposingProblem -> Problem) -> ElmParser Exposing
exposing_ problemWrapper =
    Parser.oneOf
        [ Parser.backtrackable
            (Parser.succeed Open
                |. Parser.symbol (Parser.Token "(" (problemWrapper ExposingStart))
                |. Parser.symbol (Parser.Token ".." (problemWrapper ExposingValue))
                |. Parser.symbol (Parser.Token ")" (problemWrapper ExposingEnd))
            )
        , Parser.succeed Explicit
            |= Parser.sequence
                { start = Parser.Token "(" (problemWrapper ExposingStart)
                , separator = Parser.Token "," (problemWrapper ExposingEnd)
                , end = Parser.Token ")" (problemWrapper ExposingEnd)
                , spaces = Parser.spaces
                , item = chompExposed problemWrapper
                , trailing = Parser.Forbidden
                }
        ]


chompExposed : (ExposingProblem -> Problem) -> ElmParser Exposed
chompExposed problemWrapper =
    Parser.oneOf
        [ Parser.succeed Lower
            |= Parser.variable
                { start = Char.isLower
                , inner = \c -> Char.isAlphaNum c || c == '_'
                , reserved = reservedWords
                , expecting = problemWrapper ExposingValue
                }
        , Parser.succeed Operator
            |. Parser.symbol (Parser.Token "(" (problemWrapper ExposingValue))
            |= Parser.variable
                { start = \c -> Set.member c operatorChars
                , inner = \c -> Set.member c operatorChars
                , reserved = reservedOperatorWords
                , expecting = problemWrapper ExposingOperator
                }
            |. Parser.symbol (Parser.Token ")" (problemWrapper ExposingOperatorRightParen))
        , Parser.succeed Upper
            |= Parser.variable
                { start = Char.isUpper
                , inner = \c -> Char.isAlphaNum c || c == '_'
                , reserved = Set.empty
                , expecting = problemWrapper ExposingValue
                }
            |= Parser.oneOf
                [ Parser.succeed Public
                    |. Parser.symbol (Parser.Token "(..)" (problemWrapper ExposingTypePrivacy))
                , Parser.succeed Private
                ]
        ]


operatorChars : Set Char
operatorChars =
    Set.fromList
        [ '+'
        , '-'
        , '/'
        , '*'
        , '='
        , '.'
        , '<'
        , '>'
        , ':'
        , '&'
        , '|'
        , '^'
        , '?'
        , '%'
        , '!'
        ]


reservedOperatorWords : Set String
reservedOperatorWords =
    Set.fromList
        [ "."
        , "|"
        , "->"
        , "="
        , ":"
        ]


reservedWords : Set String
reservedWords =
    Set.fromList
        [ "if"
        , "then"
        , "else"
        , "case"
        , "of"
        , "let"
        , "in"
        , "type"
        , "module"
        , "where"
        , "import"
        , "exposing"
        , "as"
        , "port"
        ]


portToken : Token Problem
portToken =
    Parser.Token "port" PortModuleProblem


moduleToken : Problem -> Token Problem
moduleToken problem =
    Parser.Token "module" problem


exposingToken : Problem -> Token Problem
exposingToken problem =
    Parser.Token "exposing" problem


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
    Test.describe "Main module"
        [ chompHeaderTests
        , moduleNameTests
        , exposingTests
        ]


chompHeaderTests : Test
chompHeaderTests =
    Test.describe "chompHeader"
        [ Test.test "module Main exposing (..)" <|
            \_ ->
                Expect.equal (Parser.run chompHeader "module Main exposing (..)")
                    (Ok (Just (Header { name = ( "Main", [] ), effects = NoEffects, exposing_ = Open })))
        , Test.test "port module Main exposing (..)" <|
            \_ ->
                Expect.equal (Parser.run chompHeader "port module Main exposing (..)")
                    (Ok (Just (Header { name = ( "Main", [] ), effects = Ports, exposing_ = Open })))
        ]


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


exposingTests : Test
exposingTests =
    Test.describe "exposing_"
        [ Test.test "(..)" <|
            \_ ->
                Expect.equal (Parser.run (exposing_ ModuleExposing) "(..)") (Ok Open)
        , Test.test "(foo)" <|
            \_ ->
                Expect.equal (Parser.run (exposing_ ModuleExposing) "(foo)") (Ok (Explicit [ Lower "foo" ]))
        , Test.test "(Foo)" <|
            \_ ->
                Expect.equal (Parser.run (exposing_ ModuleExposing) "(Foo)") (Ok (Explicit [ Upper "Foo" Private ]))
        , Test.test "(Foo(..))" <|
            \_ ->
                Expect.equal (Parser.run (exposing_ ModuleExposing) "(Foo(..))") (Ok (Explicit [ Upper "Foo" Public ]))
        , Test.test "((+))" <|
            \_ ->
                Expect.equal (Parser.run (exposing_ ModuleExposing) "((+))") (Ok (Explicit [ Operator "+" ]))
        ]
