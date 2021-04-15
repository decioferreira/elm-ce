port module Main exposing (allTests, main)

import Array exposing (Array)
import Dict exposing (Dict)
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.File as ElmSyntax
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Expect
import Json.Decode as Decode
import Parser
import Test exposing (Test)



-- PORTS


port convert : (() -> msg) -> Sub msg


port convertedSuccess : { message : String, code : String } -> Cmd msg



-- MAIN


main : Program Flags Model Msg
main =
    Platform.worker
        { init =
            \flags ->
                ( Decode.decodeValue decodeFlags flags
                    |> Result.withDefault
                        { elmCore = ""
                        , exposingModuleName = Dict.empty
                        , sourceFiles = []
                        }
                , Cmd.none
                )
        , update = update
        , subscriptions = always subscriptions
        }



-- FLAGS


type alias Flags =
    Decode.Value


decodeFlags : Decode.Decoder Model
decodeFlags =
    Decode.map2
        (\elmCore dependenciesSourceFiles ->
            let
                ( dependencies, sourceFiles ) =
                    List.unzip dependenciesSourceFiles
            in
            { elmCore = elmCore
            , exposingModuleName =
                dependencies
                    |> List.concatMap
                        (\dependency ->
                            dependency.exposingList
                                |> List.map (\exposingItem -> ( exposingItem, dependency.moduleName ))
                        )
                    |> Dict.fromList
            , sourceFiles = sourceFiles
            }
        )
        (Decode.field "elmCore" Decode.string)
        (Decode.field "sourceFiles" decodeSourceFiles)


decodeSourceFiles : Decode.Decoder (List ( Dependency, ElmFile ))
decodeSourceFiles =
    Decode.list
        (Decode.andThen
            (\sourceFile ->
                case sourceFileParser sourceFile of
                    Ok result ->
                        Decode.succeed result

                    Err error ->
                        Decode.fail (Parser.deadEndsToString error)
            )
            decodeSourceFile
        )



-- SOURCE FILES


type alias SourceFile =
    { namespace : List String
    , source : String
    }


decodeSourceFile : Decode.Decoder SourceFile
decodeSourceFile =
    Decode.map2 SourceFile
        (Decode.field "namespace" (Decode.list Decode.string))
        (Decode.field "source" Decode.string)


sourceFileParser : SourceFile -> Result (List Parser.DeadEnd) ( Dependency, ElmFile )
sourceFileParser sourceFile =
    let
        fileResult =
            Elm.Parser.parse sourceFile.source
                |> Result.map (Elm.Processing.process Elm.Processing.init)
    in
    case fileResult of
        Ok file ->
            let
                ( moduleName, exposingList ) =
                    case Node.value file.moduleDefinition of
                        Module.NormalModule moduleData ->
                            ( Node.value moduleData.moduleName
                            , Node.value moduleData.exposingList
                            )

                        Module.PortModule moduleData ->
                            ( Node.value moduleData.moduleName
                            , Node.value moduleData.exposingList
                            )

                        Module.EffectModule moduleData ->
                            ( Node.value moduleData.moduleName
                            , Node.value moduleData.exposingList
                            )
            in
            Ok
                ( { moduleName = sourceFile.namespace ++ moduleName
                  , exposingList =
                        case exposingList of
                            Exposing.All _ ->
                                Debug.todo "Exposing.All"

                            Exposing.Explicit declarations ->
                                List.concatMap
                                    (\declaration ->
                                        case Node.value declaration of
                                            Exposing.InfixExpose infixExpose ->
                                                [ "(" ++ infixExpose ++ ")" ]

                                            Exposing.FunctionExpose functionExpose ->
                                                [ functionExpose ]

                                            Exposing.TypeOrAliasExpose typeOrAliasExpose ->
                                                []

                                            Exposing.TypeExpose exposedType ->
                                                []
                                    )
                                    declarations
                  }
                , { namespace = sourceFile.namespace ++ moduleName
                  , file = file
                  }
                )

        Err error ->
            Err error



-- DEPENDENCY


type alias Dependency =
    { moduleName : ModuleName
    , exposingList : List String
    }



-- MODEL


type alias Model =
    { elmCore : String
    , exposingModuleName : Dict String ModuleName
    , sourceFiles : List ElmFile
    }


type alias ElmFile =
    { namespace : List String
    , file : ElmSyntax.File
    }



-- MSG


type Msg
    = Convert



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Convert ->
            let
                code =
                    model.sourceFiles
                        |> List.map (elmFileToCode model)
                        |> String.join ";\n"
            in
            ( model
            , convertedSuccess
                { message = "Success!"
                , code = preCode ++ model.elmCore ++ "\n" ++ code ++ "\n" ++ postCode
                }
            )


elmFileToCode : Model -> ElmFile -> String
elmFileToCode model elmFile =
    elmFile.file.declarations
        |> List.foldr
            (\declaration acc ->
                case Node.value declaration of
                    Declaration.FunctionDeclaration function ->
                        let
                            functionDeclaration =
                                Node.value function.declaration

                            ( functionArguments, functionVars ) =
                                functionDeclaration.arguments
                                    |> List.indexedMap argumentToString
                                    |> List.unzip

                            name =
                                Node.value functionDeclaration.name

                            _ =
                                ( elmFile.namespace, function.declaration )
                                    |> (if functionFullName elmFile.namespace name == "$author$project$Main$addOperatorApplication" then
                                            Debug.log "$author$project$Main$addOperatorApplication"

                                        else
                                            identity
                                       )

                            ( expressionString, expressionDependencies ) =
                                expressionToString model elmFile functionArguments functionDeclaration.expression

                            functionCode =
                                "var "
                                    ++ functionFullName elmFile.namespace name
                                    ++ " = "
                                    ++ functionWrapper functionArguments (functionShouldReturn functionDeclaration.expression) expressionString
                        in
                        Dict.insert name ( functionCode, expressionDependencies ) acc

                    Declaration.CustomTypeDeclaration { constructors } ->
                        List.foldr
                            (\(Node _ constructor) customTypeAcc ->
                                let
                                    name =
                                        Node.value constructor.name

                                    constructorArguments =
                                        constructor.arguments
                                            |> List.indexedMap (\index _ -> smallVar index)
                                in
                                Dict.insert name
                                    ( "var "
                                        ++ functionFullName elmFile.namespace name
                                        ++ " = "
                                        ++ functionWrapper constructorArguments
                                            True
                                            ("{ "
                                                ++ (constructorArguments
                                                        |> List.map (\arg -> arg ++ ": " ++ arg)
                                                        |> (::) ("$: \"" ++ name ++ "\"")
                                                        |> String.join ", "
                                                   )
                                                ++ " }"
                                            )
                                    , []
                                    )
                                    customTypeAcc
                            )
                            acc
                            constructors

                    Declaration.PortDeclaration signature ->
                        let
                            name =
                                Node.value signature.name
                        in
                        case Node.value signature.typeAnnotation of
                            TypeAnnotation.FunctionTypeAnnotation (Node _ converter) (Node _ (TypeAnnotation.Typed (Node _ ( [], "Sub" )) _)) ->
                                let
                                    arguments =
                                        case converter of
                                            TypeAnnotation.FunctionTypeAnnotation (Node _ TypeAnnotation.Unit) _ ->
                                                [ "$elm$json$Json$Decode$null(0)" ]

                                            TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( [], "Int" )) _)) _ ->
                                                [ "$elm$json$Json$Decode$int" ]

                                            TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( [], "String" )) _)) _ ->
                                                [ "$elm$json$Json$Decode$string" ]

                                            _ ->
                                                [ "/* TODO PortDeclaration(Sub) converter (" ++ Debug.toString ( name, converter ) ++ ") */" ]
                                in
                                Dict.insert name
                                    ( "var "
                                        ++ functionFullName elmFile.namespace name
                                        ++ " = _Platform_incomingPort("
                                        ++ String.join ", " (("\"" ++ name ++ "\"") :: arguments)
                                        ++ ")"
                                    , []
                                    )
                                    acc

                            TypeAnnotation.FunctionTypeAnnotation (Node _ converter) (Node _ (TypeAnnotation.Typed (Node _ ( [], "Cmd" )) _)) ->
                                let
                                    arguments =
                                        case converter of
                                            TypeAnnotation.Typed (Node _ ( [], "Int" )) _ ->
                                                [ "$elm$json$Json$Encode$int" ]

                                            TypeAnnotation.Typed (Node _ ( [], "String" )) _ ->
                                                [ "$elm$json$Json$Encode$string" ]

                                            _ ->
                                                [ "/* TODO PortDeclaration(Cmd) converter (" ++ Debug.toString ( name, converter ) ++ ") */" ]
                                in
                                Dict.insert name
                                    ( "var "
                                        ++ functionFullName elmFile.namespace name
                                        ++ " = _Platform_outgoingPort("
                                        ++ String.join ", " (("\"" ++ name ++ "\"") :: arguments)
                                        ++ ")"
                                    , []
                                    )
                                    acc

                            _ ->
                                acc

                    _ ->
                        acc
            )
            Dict.empty
        |> buildCode
        |> String.join ";\n"


buildCode : Dict String ( String, List String ) -> List String
buildCode declarations =
    declarations
        |> Dict.foldl (buildCodeHelp declarations) { added = [], result = [] }
        |> .result


buildCodeHelp : Dict String ( String, List String ) -> String -> ( String, List String ) -> { added : List String, result : List String } -> { added : List String, result : List String }
buildCodeHelp declarations declarationName ( declarationCode, dependencies ) acc =
    if List.member declarationName acc.added then
        acc

    else
        case dependencies of
            headDependency :: tailDependencies ->
                case Dict.get headDependency declarations of
                    Just dependencyDeclaration ->
                        acc
                            |> buildCodeHelp declarations headDependency dependencyDeclaration
                            |> buildCodeHelp declarations declarationName ( declarationCode, tailDependencies )

                    Nothing ->
                        buildCodeHelp declarations declarationName ( declarationCode, tailDependencies ) acc

            [] ->
                { added = declarationName :: acc.added, result = acc.result ++ [ declarationCode ] }


functionWrapper : List String -> Bool -> String -> String
functionWrapper arguments return code =
    let
        returnCode =
            if return then
                "return " ++ code ++ ";"

            else
                code
    in
    case arguments of
        [] ->
            code

        singleArgument :: [] ->
            "function(" ++ singleArgument ++ ") { " ++ returnCode ++ " }"

        _ ->
            "F" ++ String.fromInt (List.length arguments) ++ "(function(" ++ String.join ", " arguments ++ ") { " ++ returnCode ++ " })"


functionShouldReturn : Node Expression -> Bool
functionShouldReturn expression =
    case Node.value expression of
        Expression.IfBlock _ _ _ ->
            False

        Expression.LetExpression _ ->
            False

        Expression.CaseExpression _ ->
            False

        _ ->
            True


preCode : String
preCode =
    "(function(scope) {\n"


postCode : String
postCode =
    """
_Platform_export({'Main':{'init':$author$project$Main$main($elm$json$Json$Decode$succeed(0))(0)}});
}(this));
"""


argumentToString : Int -> Node Pattern -> ( String, List String )
argumentToString index argument =
    case Node.value argument of
        Pattern.AllPattern ->
            ( "_v" ++ String.fromInt index, [] )

        Pattern.VarPattern name ->
            ( name, [] )

        Pattern.NamedPattern qualifiedNameRef arguments ->
            ( "_v" ++ String.fromInt index
            , arguments
                |> List.indexedMap
                    (\namedPatternIndex (Node _ namedPatternArgument) ->
                        case namedPatternArgument of
                            Pattern.VarPattern name ->
                                "var "
                                    ++ name
                                    ++ " = "
                                    ++ "_v"
                                    ++ String.fromInt index
                                    ++ "."
                                    ++ smallVar namedPatternIndex
                                    ++ ";"

                            debugNamedPatternArgument ->
                                "/* TODO Pattern.NamedPattern (" ++ Debug.toString debugNamedPatternArgument ++ ") */"
                    )
            )

        Pattern.AsPattern asPattern (Node _ x) ->
            argumentToString index asPattern
                |> Tuple.mapFirst (\_ -> x)

        Pattern.ParenthesizedPattern parenthesizedPattern ->
            argumentToString index parenthesizedPattern

        debugArgument ->
            ( "/* TODO argumentToString (" ++ Debug.toString debugArgument ++ ") */", [] )


expressionToString : Model -> ElmFile -> List String -> Node Expression -> ( String, List String )
expressionToString model elmFile scopeVariables expression =
    case Node.value expression of
        Expression.UnitExpr ->
            ( "0", [] )

        Expression.Application (functionExpression :: arguments) ->
            let
                ( functionName, functionDependencies ) =
                    expressionToString model elmFile scopeVariables functionExpression

                ( argumentExpressions, argumentDependencies ) =
                    List.map (expressionToString model elmFile scopeVariables) arguments
                        |> List.unzip
                        |> Tuple.mapSecond List.concat
            in
            ( functionName ++ "(" ++ String.join ", " argumentExpressions ++ ")"
            , functionDependencies ++ argumentDependencies
            )

        Expression.OperatorApplication "|>" _ leftExpression rightExpression ->
            let
                ( leftString, leftDependencies ) =
                    expressionToString model elmFile scopeVariables leftExpression

                ( rightString, rightDependencies ) =
                    expressionToString model elmFile scopeVariables rightExpression
            in
            ( rightString ++ "(" ++ leftString ++ ")"
            , leftDependencies ++ rightDependencies
            )

        Expression.OperatorApplication "<|" _ leftExpression rightExpression ->
            let
                ( leftString, leftDependencies ) =
                    expressionToString model elmFile scopeVariables leftExpression

                ( rightString, rightDependencies ) =
                    expressionToString model elmFile scopeVariables rightExpression
            in
            ( leftString ++ "(" ++ rightString ++ ")"
            , leftDependencies ++ rightDependencies
            )

        Expression.OperatorApplication operator infixDirection leftExpression rightExpression ->
            let
                ( leftString, leftDependencies ) =
                    expressionToString model elmFile scopeVariables leftExpression

                ( rightString, rightDependencies ) =
                    expressionToString model elmFile scopeVariables rightExpression
            in
            ( leftString ++ " " ++ operator ++ " " ++ rightString
            , leftDependencies ++ rightDependencies
            )

        Expression.FunctionOrValue functionOrValueModuleName functionOrValue ->
            case ( Dict.get functionOrValue model.exposingModuleName, functionOrValueModuleName, List.member functionOrValue scopeVariables ) of
                ( _, [], True ) ->
                    ( functionOrValue, [] )

                ( Just moduleName, _, False ) ->
                    ( functionFullName moduleName functionOrValue, [ functionFullName moduleName functionOrValue ] )

                ( _, [], False ) ->
                    ( functionFullName elmFile.namespace functionOrValue, [ functionFullName elmFile.namespace functionOrValue ] )

                ( Nothing, [ "Cmd" ], _ ) ->
                    ( String.join "$" [ "", "elm", "core", "Platform", "Cmd", functionOrValue ], [] )

                ( Nothing, [ "Sub" ], _ ) ->
                    ( String.join "$" [ "", "elm", "core", "Platform", "Sub", functionOrValue ], [] )

                _ ->
                    ( String.join "$" ([ "", "elm", "core" ] ++ functionOrValueModuleName ++ [ functionOrValue ]), [] )

        Expression.IfBlock condition ifBlock elseBlock ->
            let
                ( conditionString, conditionDependencies ) =
                    expressionToString model elmFile scopeVariables condition

                ( ifBlockString, ifBlockDependencies ) =
                    expressionToString model elmFile scopeVariables ifBlock

                ( elseBlockString, elseBlockDependencies ) =
                    expressionToString model elmFile scopeVariables elseBlock
            in
            ( "if(" ++ conditionString ++ ") {" ++ ifBlockString ++ "} else {" ++ elseBlockString ++ "}"
            , conditionDependencies ++ ifBlockDependencies ++ elseBlockDependencies
            )

        -- Expression.PrefixOperator _ ->
        -- Expression.Operator _ ->
        Expression.Integer integer ->
            ( String.fromInt integer, [] )

        Expression.Hex hexInteger ->
            ( "0x" ++ String.fromInt hexInteger, [] )

        Expression.Floatable float ->
            ( String.fromFloat float, [] )

        Expression.Negation negationExpression ->
            expressionToString model elmFile scopeVariables negationExpression
                |> Tuple.mapFirst ((++) "-")

        Expression.Literal literal ->
            ( "\"" ++ literal ++ "\"", [] )

        Expression.CharLiteral charLiteral ->
            ( "\"" ++ String.fromChar charLiteral ++ "\"", [] )

        Expression.TupledExpression variables ->
            variables
                |> List.indexedMap
                    (\index variable ->
                        expressionToString model elmFile scopeVariables variable
                            |> Tuple.mapFirst ((++) (smallVar index ++ ": "))
                    )
                |> List.unzip
                |> Tuple.mapBoth (\varStrings -> "{ " ++ String.join ", " varStrings ++ " }") List.concat

        Expression.ParenthesizedExpression parenthesizedExpression ->
            expressionToString model elmFile scopeVariables parenthesizedExpression

        Expression.LetExpression letBlock ->
            -- TODO let declarations
            expressionToString model elmFile scopeVariables letBlock.expression

        Expression.CaseExpression caseBlock ->
            let
                ( caseBlockExpression, caseBlockDependencies ) =
                    expressionToString model elmFile scopeVariables caseBlock.expression

                ( cases, caseDependencies ) =
                    caseBlock.cases
                        |> List.indexedMap
                            (\index ( argument, block ) ->
                                let
                                    ( argumentIdentifier, argumentAssignments, argumentScopeVariables ) =
                                        case Node.value argument of
                                            Pattern.NamedPattern qualifiedNameRef arguments ->
                                                let
                                                    ( namedPatternAssignments, namedPatternScopeVariables ) =
                                                        arguments
                                                            |> List.indexedMap
                                                                (\namedPatternIndex (Node _ namedPatternArgument) ->
                                                                    case namedPatternArgument of
                                                                        Pattern.VarPattern name ->
                                                                            ( "var "
                                                                                ++ name
                                                                                ++ " = "
                                                                                ++ caseBlockExpression
                                                                                ++ "."
                                                                                ++ smallVar namedPatternIndex
                                                                                ++ ";"
                                                                            , [ name ]
                                                                            )

                                                                        debugNamedPatternArgument ->
                                                                            ( "/* TODO CaseExpression.NamedPattern (" ++ Debug.toString debugNamedPatternArgument ++ ") */", [] )
                                                                )
                                                            |> List.unzip
                                                            |> Tuple.mapSecond List.concat
                                                in
                                                ( "\"" ++ String.join "$" (qualifiedNameRef.moduleName ++ [ qualifiedNameRef.name ]) ++ "\""
                                                , String.join " " namedPatternAssignments
                                                , namedPatternScopeVariables
                                                )

                                            debugArgument ->
                                                ( "/* TODO CaseExpression (" ++ Debug.toString debugArgument ++ ") */", "", [] )

                                    ( blockString, blockDependencies ) =
                                        expressionToString model elmFile (scopeVariables ++ argumentScopeVariables) block
                                in
                                ( "if(" ++ caseBlockExpression ++ ".$ === " ++ argumentIdentifier ++ ") { " ++ argumentAssignments ++ " return " ++ blockString ++ "; }"
                                , blockDependencies
                                )
                            )
                        |> List.unzip
                        |> Tuple.mapSecond List.concat
            in
            ( String.join " else " cases
            , caseBlockDependencies ++ caseDependencies
            )

        Expression.LambdaExpression lambda ->
            let
                ( lambdaArgs, lambdaVars ) =
                    lambda.args
                        |> List.indexedMap argumentToString
                        |> List.unzip
            in
            expressionToString model elmFile lambdaArgs lambda.expression
                |> Tuple.mapFirst (\lambdaExpression -> "function(" ++ String.join ", " lambdaArgs ++ ") { return " ++ lambdaExpression ++ "; }")

        Expression.RecordExpr recordSetters ->
            recordSetters
                |> List.map
                    (\(Node _ ( Node _ name, value )) ->
                        let
                            ( valueString, valueDependencies ) =
                                expressionToString model elmFile scopeVariables value
                        in
                        ( name ++ ": " ++ valueString
                        , valueDependencies
                        )
                    )
                |> List.unzip
                |> Tuple.mapSecond List.concat
                |> Tuple.mapFirst (\recordSettersList -> "{ " ++ String.join ", " recordSettersList ++ " }")

        Expression.ListExpr listExpr ->
            List.map (expressionToString model elmFile scopeVariables) listExpr
                |> List.unzip
                |> Tuple.mapBoth
                    (\listElements -> "_List_fromArray([" ++ String.join ", " listElements ++ "])")
                    List.concat

        Expression.RecordAccess recordAccessExpression (Node _ accessor) ->
            expressionToString model elmFile scopeVariables recordAccessExpression
                |> Tuple.mapFirst (\recordAccessExpressionString -> recordAccessExpressionString ++ "." ++ accessor)

        -- Expression.RecordAccessFunction _ ->
        -- Expression.RecordUpdateExpression _ _ ->
        -- Expression.GLSLExpression _ ->
        debugExpression ->
            ( "/* TODO expressionToString (" ++ Debug.toString debugExpression ++ ") */", [] )



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    convert (\_ -> Convert)



-- HELPERS


smallVar : Int -> String
smallVar index =
    let
        prefix =
            case index // Array.length validVarChars of
                0 ->
                    ""

                division ->
                    smallVar (division - 1)

        remainder =
            remainderBy (Array.length validVarChars) index
    in
    prefix
        ++ (Array.get remainder validVarChars
                |> Maybe.map String.fromChar
                |> Maybe.withDefault ""
           )


validVarChars : Array Char
validVarChars =
    Array.fromList [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z' ]


functionFullName : ModuleName -> String -> String
functionFullName moduleName name =
    String.join "$" ("" :: moduleName ++ [ name ])



-- JAVASCRIPT


type alias JSTopLevelStatement =
    { name : String
    , value : JSTopLevelValue
    }


type JSTopLevelValue
    = JSTopLevelVariable String
    | JSTopLevelFunctionCall (List String)
    | JSTopLevelFunctionDefinition
        { arguments : List String
        }


functionImplementationToJavaScript : ModuleName -> Node Expression.FunctionImplementation -> JSTopLevelStatement
functionImplementationToJavaScript moduleName (Node _ functionImplementation) =
    { name = functionFullName moduleName (Node.value functionImplementation.name)
    , value =
        case Node.value functionImplementation.expression of
            Expression.UnitExpr ->
                JSTopLevelVariable "_Utils_Tuple0"

            Expression.Application ((Node _ (Expression.FunctionOrValue [] name)) :: arguments) ->
                JSTopLevelFunctionCall (functionFullName moduleName name :: List.map applicationArgumentToJavaScript arguments)

            _ ->
                Debug.todo "functionImplementationToJavaScript.value"
    }


applicationArgumentToJavaScript : Node Expression -> String
applicationArgumentToJavaScript argument =
    case Node.value argument of
        Expression.Integer integer ->
            String.fromInt integer

        _ ->
            Debug.todo "applicationArgumentToJavaScript"



-- JAVASCRIPT :: TESTS


functionImplementationToJavaScriptTests : Test
functionImplementationToJavaScriptTests =
    Test.describe "functionImplementationToJavaScript"
        [ Test.test "UnitExpr" <|
            \_ ->
                Expect.equal
                    (functionImplementationToJavaScript [ "author", "project", "Main" ]
                        (Node Range.emptyRange
                            { arguments = []
                            , expression = Node Range.emptyRange Expression.UnitExpr
                            , name = Node Range.emptyRange "unit"
                            }
                        )
                    )
                    { name = "$author$project$Main$unit"
                    , value = JSTopLevelVariable "_Utils_Tuple0"
                    }
        , Test.test "Application" <|
            \_ ->
                Expect.equal
                    (functionImplementationToJavaScript [ "author", "project", "Main" ]
                        (Node Range.emptyRange
                            { arguments = []
                            , expression =
                                Node Range.emptyRange
                                    (Expression.Application
                                        [ Node Range.emptyRange (Expression.FunctionOrValue [] "addOperatorApplication")
                                        , Node Range.emptyRange (Expression.Integer 1)
                                        , Node Range.emptyRange (Expression.Integer 2)
                                        ]
                                    )
                            , name = Node Range.emptyRange "addApplication"
                            }
                        )
                    )
                    { name = "$author$project$Main$addApplication"
                    , value = JSTopLevelFunctionCall [ "$author$project$Main$addOperatorApplication", "1", "2" ]
                    }
        , Test.test "OperatorApplication" <|
            \_ ->
                Expect.equal
                    (functionImplementationToJavaScript [ "author", "project", "Main" ]
                        (Node Range.emptyRange
                            { arguments =
                                [ Node Range.emptyRange (Pattern.VarPattern "a")
                                , Node Range.emptyRange (Pattern.VarPattern "b")
                                ]
                            , expression =
                                Node Range.emptyRange
                                    (Expression.OperatorApplication "+"
                                        Infix.Left
                                        (Node Range.emptyRange (Expression.FunctionOrValue [] "a"))
                                        (Node Range.emptyRange (Expression.FunctionOrValue [] "b"))
                                    )
                            , name = Node Range.emptyRange "addOperatorApplication"
                            }
                        )
                    )
                    { name = "$author$project$Main$addOperatorApplication"
                    , value =
                        JSTopLevelFunctionDefinition
                            { arguments = [ "a", "b" ]

                            -- TODO
                            }
                    }
        ]



-- TESTS


allTests : Test
allTests =
    Test.describe "Elm Community Edition"
        [ argumentToStringTests
        , functionImplementationToJavaScriptTests
        , smallVarTests
        ]


argumentToStringTests : Test
argumentToStringTests =
    Test.describe "argumentToString"
        [ Test.test "Pattern.AllPattern" <|
            \_ ->
                Expect.equal
                    (argumentToString 0 (Node Range.emptyRange Pattern.AllPattern))
                    ( "_v0", [] )
        ]


smallVarTests : Test
smallVarTests =
    Test.describe "smallVar"
        [ Test.test "0 -> a" <|
            \_ ->
                Expect.equal (smallVar 0) "a"
        , Test.test "25 -> z" <|
            \_ ->
                Expect.equal (smallVar 25) "z"
        , Test.test "26 -> aa" <|
            \_ ->
                Expect.equal (smallVar 26) "aa"
        , Test.test "51 -> az" <|
            \_ ->
                Expect.equal (smallVar 51) "az"
        , Test.test "52 -> ba" <|
            \_ ->
                Expect.equal (smallVar 52) "ba"
        ]
