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
                                { message = "Detected problems in 1 module."
                                , error = """-- EXPECTING MODULE NAME - specs/assets/elm/Parser/ModuleHeader/lowercaseName.elm

I was parsing an `module` declaration until I got stuck here:

1| module lowercaseName exposing (foo)
          ^
I was expecting to see the module name next, like in these examples:

    module Dict exposing (..)
    module Maybe exposing (..)
    module Html.Attributes exposing (..)
    module Json.Decode exposing (..)

Notice that the module names all start with capital letters. That is required!
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
    = ModuleProblem
    | ModuleName
    | ModuleExposing ExposingProblem
      --
    | PortModuleProblem
    | PortModuleName
    | PortModuleExposing ExposingProblem
      --
    | FreshLine Row Col
      --
    | ImportName
    | ImportAs
    | ImportAlias
    | ImportExposing
    | ImportExposingList ExposingProblem
      --
    | InfixProblem
      --
    | Declarations DeclProblem


type ExposingProblem
    = ExposingStart
    | ExposingValue
    | ExposingOperator
    | ExposingOperatorRightParen
    | ExposingTypePrivacy
    | ExposingEnd



-- PROBLEM : DECLARATIONS


type DeclProblem
    = DeclStart
    | DeclSpace SpaceProblem Row Col
      --
      -- | Port Port Row Col
      -- | DeclType DeclType Row Col
      -- | DeclDef Name.Name DeclDef Row Col
      --
    | DeclFreshLineAfterDocComment Row Col



-- PROBLEM MISC


type SpaceProblem
    = HasTab
    | EndlessMultiComment



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
        , imports : List Import
        , infixes : List Infix
        , decls : List Decl
        }


chompModule : ElmParser Module
chompModule =
    Parser.succeed
        (\header imports infixes decls ->
            Module
                { header = header
                , imports = imports
                , infixes = infixes
                , decls = decls
                }
        )
        |= chompHeader
        |= chompImports
        |= chompInfixes
        |= chompDeclarations



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
            |= lowerVariable (problemWrapper ExposingValue)
        , Parser.succeed Operator
            |. Parser.symbol (Parser.Token "(" (problemWrapper ExposingValue))
            |= operatorVariable (problemWrapper ExposingOperator)
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


lowerVariable : Problem -> ElmParser String
lowerVariable expecting =
    Parser.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = reservedWords
        , expecting = expecting
        }


operatorVariable : Problem -> ElmParser String
operatorVariable expecting =
    Parser.variable
        { start = \c -> Set.member c operatorChars
        , inner = \c -> Set.member c operatorChars
        , reserved = reservedOperatorWords
        , expecting = expecting
        }


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


importToken : Problem -> Token Problem
importToken problem =
    Parser.Token "import" problem


asToken : Problem -> Token Problem
asToken problem =
    Parser.Token "as" problem


infixToken : Problem -> Token Problem
infixToken problem =
    Parser.Token "infix" problem


leftToken : Problem -> Token Problem
leftToken problem =
    Parser.Token "left" problem


rightToken : Problem -> Token Problem
rightToken problem =
    Parser.Token "right" problem


nonToken : Problem -> Token Problem
nonToken problem =
    Parser.Token "non" problem


typeToken : Problem -> Token Problem
typeToken problem =
    Parser.Token "type" problem


aliasToken : Problem -> Token Problem
aliasToken problem =
    Parser.Token "alias" problem


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



-- IMPORTS


type Import
    = Import
        -- Import { _import :: A.Located Name , _alias :: Maybe Name , _exposing :: Exposing }
        { import_ : ( String, List String )
        , alias_ : Maybe String
        , exposing_ : Exposing
        }


chompImports : ElmParser (List Import)
chompImports =
    Parser.loop [] chompImportsHelp


chompImportsHelp : List Import -> ElmParser (Step (List Import) (List Import))
chompImportsHelp imports =
    Parser.oneOf
        [ Parser.succeed (\import_ -> Loop (import_ :: imports))
            |= chompImport
        , Parser.succeed ()
            |> Parser.map (\_ -> Done (List.reverse imports))
        ]


chompImport : ElmParser Import
chompImport =
    Parser.succeed
        (\start import_ alias_ importExposing ->
            Import
                { import_ = import_
                , alias_ = alias_
                , exposing_ = importExposing
                }
        )
        |. freshLine
        |= Parser.getPosition
        |. Parser.keyword (importToken ImportName)
        |. Parser.spaces
        |= moduleName
        |= Parser.oneOf
            [ Parser.backtrackable
                (Parser.succeed Just
                    |. Parser.spaces
                    |. Parser.keyword (asToken ImportAs)
                    |. Parser.spaces
                    |= moduleNameWithoutDots
                )
            , Parser.succeed Nothing
            ]
        |= Parser.oneOf
            [ Parser.backtrackable
                (Parser.succeed identity
                    |. Parser.spaces
                    |. Parser.keyword (exposingToken ImportExposing)
                    |. Parser.spaces
                    |= exposing_ ImportExposingList
                )
            , Parser.succeed (Explicit [])
            ]



-- INFIXES


type Infix
    = Infix
        -- Infix Name Binop.Associativity Binop.Precedence Name
        { op : String
        , associativity : Associativity
        , precedence : Int
        , name : String
        }


type Associativity
    = Left
    | Non
    | Right


chompInfixes : ElmParser (List Infix)
chompInfixes =
    Parser.loop [] chompInfixesHelp


chompInfixesHelp : List Infix -> ElmParser (Step (List Infix) (List Infix))
chompInfixesHelp infixes =
    Parser.oneOf
        [ Parser.succeed (\infix -> Loop (infix :: infixes))
            |= chompInfix
        , Parser.succeed ()
            |> Parser.map (\_ -> Done (List.reverse infixes))
        ]


chompInfix : ElmParser Infix
chompInfix =
    Parser.succeed
        (\start associativity precedence op name ->
            Infix
                { op = op
                , associativity = associativity
                , precedence = precedence
                , name = name
                }
        )
        |. freshLine
        |= Parser.getPosition
        |. Parser.keyword (infixToken InfixProblem)
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.map (\_ -> Left) (Parser.keyword (leftToken InfixProblem))
            , Parser.map (\_ -> Right) (Parser.keyword (rightToken InfixProblem))
            , Parser.map (\_ -> Non) (Parser.keyword (nonToken InfixProblem))
            ]
        |. Parser.spaces
        |= Parser.int InfixProblem InfixProblem
        |. Parser.spaces
        |. Parser.symbol (Parser.Token "(" InfixProblem)
        |= operatorVariable InfixProblem
        |. Parser.symbol (Parser.Token ")" InfixProblem)
        |. Parser.spaces
        |. Parser.symbol (Parser.Token "=" InfixProblem)
        |. Parser.spaces
        |= lowerVariable InfixProblem



-- DECLARATIONS


type
    Decl
    -- = Value (Maybe Src.Comment) (A.Located Src.Value)
    -- | Union (Maybe Src.Comment) (A.Located Src.Union)
    -- | Alias (Maybe Src.Comment) (A.Located Src.Alias)
    -- | Port (Maybe Src.Comment) Src.Port
    = ValueDecl (Maybe Comment) Value
    | UnionDecl (Maybe Comment) Union
    | AliasDecl (Maybe Comment) Alias
    | PortDecl (Maybe Comment) Port


type Value
    = -- Value (A.Located Name) [Pattern] Expr (Maybe Type)
      Value
        { name : String
        , args : List Pattern
        , body : Expr
        , type_ : Maybe Type
        }


type Union
    = -- Union (A.Located Name) [A.Located Name] [(A.Located Name, [Type])]
      Union
        { name : String
        , args : List String
        , variants : List ( String, List Type )
        }


type Alias
    = -- Alias (A.Located Name) [A.Located Name] Type
      Alias
        { name : String
        , args : List String
        , type_ : Type
        }


type Port
    = -- Port (A.Located Name) Type
      Port
        { name : String
        , type_ : Type
        }


chompDeclarations : ElmParser (List Decl)
chompDeclarations =
    Parser.loop [] chompDeclarationsHelp


chompDeclarationsHelp : List Decl -> ElmParser (Step (List Decl) (List Decl))
chompDeclarationsHelp declarations =
    Parser.oneOf
        [ Parser.succeed (\decl -> Loop (decl :: declarations))
            |= chompDeclaration
        , Parser.succeed ()
            |> Parser.map (\_ -> Done (List.reverse declarations))
        ]


chompDeclaration : ElmParser Decl
chompDeclaration =
    Parser.oneOf
        [ typeDecl

        -- , portDecl
        -- , valueDecl
        ]


typeDecl : ElmParser Decl
typeDecl =
    Parser.succeed
        (\start typeDeclaration ->
            typeDeclaration
        )
        |. freshLine
        |= Parser.getPosition
        |. Parser.keyword (typeToken (Declarations DeclStart))
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed
                (\( name, args ) ->
                    AliasDecl Nothing
                        (Alias
                            { name = name
                            , args = args
                            , type_ =
                                { start = ( 0, 0 )
                                , value = TUnit
                                , end = ( 0, 0 )
                                }
                            }
                        )
                )
                |. Parser.keyword (aliasToken (Declarations DeclStart))
                |= chompAliasNameToEquals
            ]


chompAliasNameToEquals : ElmParser ( String, List String )
chompAliasNameToEquals =
    Debug.todo "chompAliasNameToEquals"



-- COMMENTS


type Comment
    = Comment



-- EXPRESSIONS


type alias Expr =
    Located Expr_


type Expr_
    = Chr Char
    | Str String
    | Int Int
    | Float Float
    | Var VarType String
    | VarQual VarType String String
    | ListExpr (List Expr)
    | Op String
    | Negate Expr
    | Binops (List ( Expr, Located String )) Expr
    | Lambda (List Pattern) Expr
    | Call Expr (List Expr)
    | If (List ( Expr, Expr )) Expr
    | Let (List (Located Def)) Expr
    | Case Expr (List ( Pattern, Expr ))
    | Accessor String
    | Access Expr (Located String)
    | Update (Located String) (List ( Located String, Expr ))
    | RecordExpr (List ( Located String, Expr ))
    | Unit
    | Tuple Expr Expr (List Expr)
    | Shader ShaderSource ShaderTypes


type VarType
    = LowVar
    | CapVar



-- DEFINITIONS


type Def
    = Define (Located String) (List Pattern) Expr (Maybe Type)
    | Destruct Pattern Expr



-- PATTERN


type alias Pattern =
    Located Pattern_


type Pattern_
    = PAnything
    | PVar String
    | PRecord (List (Located String))
    | PAlias Pattern (Located String)
    | PUnit
    | PTuple Pattern Pattern (List Pattern)
    | PCtor Region String (List Pattern)
    | PCtorQual Region String String (List Pattern)
    | PList (List Pattern)
    | PCons Pattern Pattern
    | PChr String
    | PStr String
    | PInt Int



-- TYPE


type alias Type =
    Located Type_


type Type_
    = TLambda Type Type
    | TVar String
    | TType Region String (List Type)
    | TTypeQual Region String String (List Type)
    | TRecord (List ( Located String, Type )) (Maybe (Located String))
    | TUnit
    | TTuple Type Type (List Type)



-- POSITION


type Position
    = -- Position
      --   {-# UNPACK #-} !Word16
      --   {-# UNPACK #-} !Word16
      -- deriving (Eq)
      Position



-- REGION


type Region
    = -- Region Position Position
      Region Position Position



-- SHADER


type ShaderSource
    = ShaderSource


type ShaderTypes
    = ShaderTypes



-- TESTS


allTests : Test
allTests =
    Test.describe "Parser"
        [ chompHeaderTests
        , moduleNameTests
        , exposingTests
        , chompImportsTests
        , chompImportTests
        , chompInfixesTests
        , chompInfixTests
        , chompDeclarationsTests
        , chompDeclarationTests
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


chompImportsTests : Test
chompImportsTests =
    Test.describe "chompImports"
        [ Test.test "multiple imports" <|
            \_ ->
                Expect.equal (Parser.run chompImports "import Basics exposing (..)\nimport Debug")
                    (Ok
                        [ Import { import_ = ( "Basics", [] ), alias_ = Nothing, exposing_ = Open }
                        , Import { import_ = ( "Debug", [] ), alias_ = Nothing, exposing_ = Explicit [] }
                        ]
                    )
        ]


chompImportTests : Test
chompImportTests =
    Test.describe "chompImport"
        [ Test.test "import Basics" <|
            \_ ->
                Expect.equal (Parser.run chompImport "import Basics")
                    (Ok (Import { import_ = ( "Basics", [] ), alias_ = Nothing, exposing_ = Explicit [] }))
        , Test.test "import Basics exposing (..)" <|
            \_ ->
                Expect.equal (Parser.run chompImport "import Basics exposing (..)")
                    (Ok (Import { import_ = ( "Basics", [] ), alias_ = Nothing, exposing_ = Open }))
        , Test.test "import Platform.Cmd as Cmd exposing (Cmd)" <|
            \_ ->
                Expect.equal (Parser.run chompImport "import Platform.Cmd as Cmd exposing (Cmd)")
                    (Ok (Import { import_ = ( "Platform", [ "Cmd" ] ), alias_ = Just "Cmd", exposing_ = Explicit [ Upper "Cmd" Private ] }))
        , Test.test "import Foo.Bar as FooBar" <|
            \_ ->
                Expect.equal (Parser.run chompImport "import Foo.Bar as FooBar")
                    (Ok (Import { import_ = ( "Foo", [ "Bar" ] ), alias_ = Just "FooBar", exposing_ = Explicit [] }))
        ]


chompInfixesTests : Test
chompInfixesTests =
    Test.describe "chompInfixes"
        [ Test.test "multiple infixes" <|
            \_ ->
                Expect.equal (Parser.run chompInfixes "infix right 0 (<|) = apL\ninfix left  0 (|>) = apR")
                    (Ok
                        [ Infix { op = "<|", associativity = Right, precedence = 0, name = "apL" }
                        , Infix { op = "|>", associativity = Left, precedence = 0, name = "apR" }
                        ]
                    )
        ]


chompInfixTests : Test
chompInfixTests =
    Test.describe "chompInfix"
        [ Test.test "infix right 0 (<|) = apL" <|
            \_ ->
                Expect.equal (Parser.run chompInfix "infix right 0 (<|) = apL")
                    (Ok (Infix { op = "<|", associativity = Right, precedence = 0, name = "apL" }))
        , Test.test "infix left  0 (|>) = apR" <|
            \_ ->
                Expect.equal (Parser.run chompInfix "infix left  0 (|>) = apR")
                    (Ok (Infix { op = "|>", associativity = Left, precedence = 0, name = "apR" }))
        , Test.test "infix non   4 (==) = eq" <|
            \_ ->
                Expect.equal (Parser.run chompInfix "infix non   4 (==) = eq")
                    (Ok (Infix { op = "==", associativity = Non, precedence = 4, name = "eq" }))
        ]


chompDeclarationsTests : Test
chompDeclarationsTests =
    Test.describe "chompDeclarations"
        [ Test.test "multiple declarations" <|
            \_ ->
                Expect.equal (Parser.run chompDeclarations "TODO")
                    (Ok [])
        ]


chompDeclarationTests : Test
chompDeclarationTests =
    Test.describe "chompDeclaration"
        [ Test.test "type alias Foo = ()" <|
            \_ ->
                Expect.equal (Parser.run chompDeclaration "type alias Foo = ()")
                    (Ok
                        (AliasDecl Nothing
                            (Alias
                                { name = "Foo"
                                , args = []
                                , type_ =
                                    { start = ( 0, 0 )
                                    , value = TUnit
                                    , end = ( 0, 0 )
                                    }
                                }
                            )
                        )
                    )
        ]
