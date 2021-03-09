port module Main exposing (allTests, main)

import Array exposing (Array)
import Dict exposing (Dict)
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Expect
import Test exposing (Test)



-- PORTS


port convert : (String -> msg) -> Sub msg


port convertedSuccess : { message : String, code : String } -> Cmd msg


port convertedFail : { message : String, error : String } -> Cmd msg



-- MAIN


main : Program String Model Msg
main =
    Platform.worker
        { init = \code -> ( code, Cmd.none )
        , update = update
        , subscriptions = always subscriptions
        }



-- MODEL


type alias Model =
    String



-- MSG


type Msg
    = Convert String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Convert source ->
            let
                result =
                    source
                        |> Elm.Parser.parse
                        |> Result.map (Elm.Processing.process Elm.Processing.init)
            in
            case result of
                Ok file ->
                    let
                        code =
                            file.declarations
                                |> List.foldr
                                    (\declaration acc ->
                                        case Node.value declaration of
                                            Declaration.FunctionDeclaration function ->
                                                let
                                                    functionDeclaration =
                                                        Node.value function.declaration

                                                    functionArguments =
                                                        functionDeclaration.arguments
                                                            |> List.indexedMap argumentToString

                                                    name =
                                                        Node.value functionDeclaration.name

                                                    ( expressionString, expressionDependencies ) =
                                                        expressionToString functionArguments functionDeclaration.expression

                                                    functionCode =
                                                        "var $author$project$Main$"
                                                            ++ name
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
                                                            ( "var $author$project$Main$"
                                                                ++ name
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

                                                                    _ ->
                                                                        [ "/* TODO PortDeclaration(Sub) converter (" ++ Debug.toString ( name, converter ) ++ ") */" ]
                                                        in
                                                        Dict.insert name
                                                            ( "var $author$project$Main$"
                                                                ++ name
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

                                                                    _ ->
                                                                        [ "/* TODO PortDeclaration(Cmd) converter (" ++ Debug.toString ( name, converter ) ++ ") */" ]
                                                        in
                                                        Dict.insert name
                                                            ( "var $author$project$Main$"
                                                                ++ name
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
                    in
                    ( model
                    , convertedSuccess
                        { message = "Success! Compiled 1 module."
                        , code = preCode ++ model ++ "\n" ++ code ++ "\n" ++ postCode
                        }
                    )

                Err error ->
                    ( model, convertedFail { message = "", error = "TODO: MULTIPLE ERRORS!" } )


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


argumentToString : Int -> Node Pattern -> String
argumentToString index argument =
    case Node.value argument of
        Pattern.AllPattern ->
            "_v" ++ String.fromInt index

        Pattern.VarPattern name ->
            name

        Pattern.NamedPattern qualifiedNameRef arguments ->
            String.join "$" (qualifiedNameRef.moduleName ++ [ qualifiedNameRef.name ])
                ++ "("
                ++ String.join ", " (List.indexedMap argumentToString arguments)
                ++ ")"

        debugArgument ->
            "/* TODO argumentToString (" ++ Debug.toString debugArgument ++ ") */"


expressionToString : List String -> Node Expression -> ( String, List String )
expressionToString scopeVariables expression =
    case Node.value expression of
        Expression.UnitExpr ->
            ( "0"
            , []
            )

        Expression.ListExpr listExpr ->
            let
                ( listElements, dependencies ) =
                    List.map (expressionToString scopeVariables) listExpr
                        |> List.unzip
                        |> Tuple.mapSecond List.concat
            in
            ( "_List_fromArray([" ++ String.join ", " listElements ++ "])"
            , dependencies
            )

        Expression.Application (functionExpression :: arguments) ->
            let
                ( functionName, functionDependencies ) =
                    expressionToString scopeVariables functionExpression

                ( argumentExpressions, argumentDependencies ) =
                    List.map (expressionToString scopeVariables) arguments
                        |> List.unzip
                        |> Tuple.mapSecond List.concat
            in
            ( functionName ++ "(" ++ String.join ", " argumentExpressions ++ ")"
            , functionDependencies ++ argumentDependencies
            )

        Expression.OperatorApplication operator _ leftExpression rightExpression ->
            let
                ( leftString, leftDependencies ) =
                    expressionToString scopeVariables leftExpression

                ( rightString, rightDependencies ) =
                    expressionToString scopeVariables rightExpression
            in
            ( leftString ++ " " ++ operator ++ " " ++ rightString
            , leftDependencies ++ rightDependencies
            )

        Expression.FunctionOrValue moduleName functionOrValue ->
            let
                ( path, dependencies ) =
                    case ( moduleName, List.member functionOrValue scopeVariables ) of
                        ( [], True ) ->
                            ( [], [] )

                        ( [], False ) ->
                            ( [ "", "author", "project", "Main" ], [ functionOrValue ] )

                        ( [ "Cmd" ], _ ) ->
                            ( [ "", "elm", "core", "Platform", "Cmd" ], [] )

                        ( [ "Sub" ], _ ) ->
                            ( [ "", "elm", "core", "Platform", "Sub" ], [] )

                        _ ->
                            ( [ "", "elm", "core" ] ++ moduleName, [] )
            in
            ( path
                ++ [ functionOrValue ]
                |> String.join "$"
            , dependencies
            )

        Expression.Integer integer ->
            ( String.fromInt integer
            , []
            )

        Expression.TupledExpression variables ->
            variables
                |> List.indexedMap
                    (\index variable ->
                        expressionToString scopeVariables variable
                            |> Tuple.mapFirst ((++) (smallVar index ++ ": "))
                    )
                |> List.unzip
                |> Tuple.mapBoth (\varStrings -> "{ " ++ String.join ", " varStrings ++ " }") List.concat

        Expression.ParenthesizedExpression parenthesizedExpression ->
            expressionToString scopeVariables parenthesizedExpression

        Expression.CaseExpression caseBlock ->
            let
                ( caseBlockExpression, caseBlockDependencies ) =
                    expressionToString scopeVariables caseBlock.expression

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
                                        expressionToString (scopeVariables ++ argumentScopeVariables) block
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
                lambdaArgs =
                    List.indexedMap argumentToString lambda.args

                ( lambdaExpression, lambdaDependencies ) =
                    expressionToString lambdaArgs lambda.expression
            in
            ( "function(" ++ String.join ", " lambdaArgs ++ ") { return " ++ lambdaExpression ++ "; }"
            , lambdaDependencies
            )

        Expression.RecordExpr recordSetters ->
            let
                ( recordSettersList, recordSettersDependencies ) =
                    recordSetters
                        |> List.map
                            (\(Node _ ( Node _ name, value )) ->
                                let
                                    ( valueString, valueDependencies ) =
                                        expressionToString scopeVariables value
                                in
                                ( name ++ ": " ++ valueString
                                , valueDependencies
                                )
                            )
                        |> List.unzip
                        |> Tuple.mapSecond List.concat
            in
            ( "{ " ++ String.join ", " recordSettersList ++ " }"
            , recordSettersDependencies
            )

        debugExpression ->
            ( "/* TODO expressionToString (" ++ Debug.toString debugExpression ++ ") */"
            , []
            )



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    convert Convert



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



-- TESTS


allTests : Test
allTests =
    Test.describe "Elm Community Edition"
        [ argumentToStringTests
        , smallVarTests
        ]


argumentToStringTests : Test
argumentToStringTests =
    Test.describe "argumentToString"
        [ Test.test "Pattern.AllPattern" <|
            \_ ->
                Expect.equal
                    (argumentToString 0 (Node Range.emptyRange Pattern.AllPattern))
                    "_v0"
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
