port module Main exposing (allTests, main)

import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range
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
                        generatedCode =
                            file.declarations
                                |> List.foldr
                                    (\declaration acc ->
                                        case Node.value declaration of
                                            -- Declaration.PortDeclaration signature ->
                                            --     let
                                            --         _ =
                                            --             Debug.log "signature" ( Node.value signature.name, Node.value signature.typeAnnotation )
                                            --     in
                                            --     ()
                                            Declaration.FunctionDeclaration function ->
                                                -- Some inspiration can be found at https://github.com/elm-in-elm/compiler/blob/master/src/Stage/Emit/JavaScript.elm#L51
                                                let
                                                    functionDeclaration =
                                                        Node.value function.declaration

                                                    _ =
                                                        Debug.log (Node.value functionDeclaration.name) functionDeclaration

                                                    functionArguments =
                                                        functionDeclaration.arguments
                                                            |> List.indexedMap argumentToString
                                                            |> String.join ", "

                                                    functionString =
                                                        "function "
                                                            ++ Node.value functionDeclaration.name
                                                            ++ "("
                                                            ++ functionArguments
                                                            ++ ") { return "
                                                            ++ expressionToString functionDeclaration.expression
                                                            ++ "; }"
                                                in
                                                functionString :: acc

                                            _ ->
                                                acc
                                    )
                                    []
                                |> String.join "\n"
                    in
                    ( model, convertedSuccess { message = "Success! Compiled 1 module.", code = coreCode ++ "\n\n" ++ generatedCode } )

                Err error ->
                    ( model, convertedFail { message = "", error = "TODO: MULTIPLE ERRORS!" } )


coreCode : String
coreCode =
    """// Source: https://github.com/thunklife/fn-curry
function curry(fn, fnLength) {
  fnLength = fnLength || fn.length;

  return function makeCurry() {
    var args = Array.prototype.slice.call(arguments);
    if(args.length === fnLength) return fn.apply(this, args);
    return function(){
      var newArgs = Array.prototype.slice.call(arguments);
      return makeCurry.apply(this, args.concat(newArgs));
    }
  }
};

this.Elm = {
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


argumentToString : Int -> Node Pattern -> String
argumentToString index argument =
    case Node.value argument of
        Pattern.AllPattern ->
            "_v" ++ String.fromInt index

        Pattern.VarPattern name ->
            name

        Pattern.NamedPattern qualifiedNameRef _ ->
            "/* TODO NamedPattern */"

        _ ->
            "/* TODO argumentToString */"


expressionToString : Node Expression -> String
expressionToString expression =
    case Node.value expression of
        Expression.UnitExpr ->
            "0"

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

        _ ->
            "/* TODO expressionToString */"



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    convert Convert



-- TESTS


allTests : Test
allTests =
    Test.describe "Elm Community Edition"
        [ argumentToStringTests
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
