port module Main exposing (allTests, main)

import Array exposing (Array)
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
                                                            |> String.join ", "

                                                    functionCode =
                                                        "const "
                                                            ++ Node.value functionDeclaration.name
                                                            ++ " = curry(function "
                                                            ++ Node.value functionDeclaration.name
                                                            ++ "("
                                                            ++ functionArguments
                                                            ++ ") { return "
                                                            ++ expressionToString functionDeclaration.expression
                                                            ++ "; })"
                                                in
                                                functionCode :: acc

                                            Declaration.CustomTypeDeclaration { constructors } ->
                                                List.foldr
                                                    (\(Node _ constructor) customTypeAcc ->
                                                        let
                                                            constructorArguments =
                                                                constructor.arguments
                                                                    |> List.indexedMap (\index _ -> smallVar index)
                                                        in
                                                        ("const "
                                                            ++ Node.value constructor.name
                                                            ++ " = curry(function "
                                                            ++ Node.value constructor.name
                                                            ++ "("
                                                            ++ String.join ", " constructorArguments
                                                            ++ ") { return { "
                                                            ++ (constructorArguments
                                                                    |> List.map (\arg -> arg ++ ": " ++ arg)
                                                                    |> (::) ("$: \"" ++ Node.value constructor.name ++ "\"")
                                                                    |> String.join ", "
                                                               )
                                                            ++ " }; })"
                                                        )
                                                            :: customTypeAcc
                                                    )
                                                    acc
                                                    constructors

                                            Declaration.PortDeclaration signature ->
                                                case Node.value signature.typeAnnotation of
                                                    TypeAnnotation.FunctionTypeAnnotation _ (Node _ (TypeAnnotation.Typed (Node _ ( [], "Sub" )) _)) ->
                                                        ("const "
                                                            ++ Node.value signature.name
                                                            ++ " = _Platform_incomingPort(\""
                                                            ++ Node.value signature.name
                                                            -- TODO missing converter
                                                            ++ "\")"
                                                        )
                                                            :: acc

                                                    TypeAnnotation.FunctionTypeAnnotation _ (Node _ (TypeAnnotation.Typed (Node _ ( [], "Cmd" )) _)) ->
                                                        ("const "
                                                            ++ Node.value signature.name
                                                            ++ " = _Platform_outgoingPort(\""
                                                            ++ Node.value signature.name
                                                            -- TODO missing converter
                                                            ++ "\")"
                                                        )
                                                            :: acc

                                                    _ ->
                                                        acc

                                            _ ->
                                                acc
                                    )
                                    []
                                |> String.join "\n"
                    in
                    ( model
                    , convertedSuccess
                        { message = "Success! Compiled 1 module."
                        , code = preCode ++ code ++ postCode
                        }
                    )

                Err error ->
                    ( model, convertedFail { message = "", error = "TODO: MULTIPLE ERRORS!" } )


preCode : String
preCode =
    """(function (scope) {

// this.Elm = {
//   Main: {
//     init: function() {
//       return {
//         ports: {
//           incomingOnePlusOne: {
//             send: function() {}
//           },
//           outgoingOnePlusOne: {
//             subscribe: function(callback) {
//               callback(2);
//             }
//           },
//           incomingAddOne: {
//             send: function() {}
//           },
//           outgoingAddOne: {
//             subscribe: function(callback) {
//               callback(3);
//             }
//           }
//         }
//       };
//     }
//   }
// };

// Curry function (source: https://github.com/thunklife/fn-curry)

function curry(fn, fnLength) {
  fnLength = fnLength || fn.length;

  return function makeCurry() {
    var args = Array.prototype.slice.call(arguments);
    if(args.length === fnLength) return fn.apply(this, args);

    return function() {
      var newArgs = Array.prototype.slice.call(arguments);
      return makeCurry.apply(this, args.concat(newArgs));
    }
  }
};

// CORE DECODERS

function _Json_succeed(msg) { return { $: "succeed", a: msg }; }

// PROGRAMS

var Platform$worker = curry(function(impl, flagDecoder, debugMetadata, args) {
});

// OUTGOING PORTS

function _Platform_outgoingPort(name, converter) {
}

// INCOMING PORTS

function _Platform_incomingPort(name, converter) {
}

// MAIN

"""


postCode : String
postCode =
    """
scope.Elm = { Main: { init: main()(_Json_succeed(0), 0) } };

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
            -- TODO
            String.join "$" (qualifiedNameRef.moduleName ++ [ qualifiedNameRef.name ])
                ++ "("
                ++ String.join ", " (List.indexedMap argumentToString arguments)
                ++ ")"

        debugArgument ->
            "/* TODO argumentToString (" ++ Debug.toString debugArgument ++ ") */"


expressionToString : Node Expression -> String
expressionToString expression =
    case Node.value expression of
        Expression.UnitExpr ->
            "0"

        Expression.ListExpr listExpr ->
            "[" ++ String.join ", " (List.map expressionToString listExpr) ++ "]"

        Expression.Application (functionExpression :: arguments) ->
            expressionToString functionExpression ++ "(" ++ String.join ", " (List.map expressionToString arguments) ++ ")"

        Expression.OperatorApplication operator _ leftExpression rightExpression ->
            "(" ++ expressionToString leftExpression ++ " " ++ operator ++ " " ++ expressionToString rightExpression ++ ")"

        Expression.FunctionOrValue moduleName functionOrValue ->
            String.join "$" (moduleName ++ [ functionOrValue ])

        Expression.Integer integer ->
            String.fromInt integer

        Expression.TupledExpression (a :: b :: []) ->
            "{ a: " ++ expressionToString a ++ ", b: " ++ expressionToString b ++ " }"

        Expression.TupledExpression (a :: b :: c :: []) ->
            "{ a: " ++ expressionToString a ++ ", b: " ++ expressionToString b ++ ", c: " ++ expressionToString c ++ " }"

        Expression.ParenthesizedExpression parenthesizedExpression ->
            expressionToString parenthesizedExpression

        Expression.CaseExpression caseBlock ->
            "(function() {"
                ++ (caseBlock.cases
                        |> List.indexedMap
                            (\index ( argument, block ) ->
                                "if(" ++ expressionToString caseBlock.expression ++ " === " ++ argumentToString index argument ++ ") { return " ++ expressionToString block ++ "; }"
                            )
                        |> String.join " else "
                   )
                ++ "})()"

        Expression.LambdaExpression lambda ->
            let
                lambdaArgs =
                    lambda.args
                        |> List.indexedMap argumentToString
                        |> String.join ", "
            in
            "function(" ++ lambdaArgs ++ ") { return " ++ expressionToString lambda.expression ++ "; }"

        Expression.RecordExpr recordSetters ->
            "{ "
                ++ String.join ", "
                    (recordSetters
                        |> List.map (\(Node _ ( Node _ name, value )) -> name ++ ": " ++ expressionToString value)
                    )
                ++ " }"

        debugExpression ->
            "/* TODO expressionToString (" ++ Debug.toString debugExpression ++ ") */"



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
