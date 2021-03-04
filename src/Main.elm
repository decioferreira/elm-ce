port module Main exposing (main)

import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Node as Node



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
                                                Node.value (Node.value function.declaration).name
                                                    :: acc

                                            _ ->
                                                acc
                                    )
                                    []
                                |> String.join "\n"

                        code =
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
                    in
                    ( model, convertedSuccess { message = "Success! Compiled 1 module.", code = generatedCode ++ "\n\n" ++ code } )

                Err error ->
                    ( model, convertedFail { message = "", error = "TODO: MULTIPLE ERRORS!" } )



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    convert Convert
