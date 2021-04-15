port module Main exposing (main)

import Array
import Helper
import Nested.Something as NestedSomething



-- INCOMING MESSAGES


port incomingAddOne : (Int -> msg) -> Sub msg


port incomingOnePlusOne : (() -> msg) -> Sub msg


port incomingAnswer : (() -> msg) -> Sub msg


port incomingQuickBrownFox : (() -> msg) -> Sub msg



-- OUTGOING MESSAGES


port outgoingAddOne : Int -> Cmd msg


port outgoingOnePlusOne : Int -> Cmd msg


port outgoingAnswer : Int -> Cmd msg


port outgoingQuickBrownFox : String -> Cmd msg



-- FUNCTIONALITY


addOne : Int -> Int
addOne a =
    1 + a


onePlusOne : Int
onePlusOne =
    1 + 1



-- SYNTAX


syntaxExamples : ()
syntaxExamples =
    let
        _ =
            { unit = unit
            , addApplication = addApplication
            , addOperatorApplication = addOperatorApplication
            }
    in
    ()


unit : ()
unit =
    ()


addApplication : Int
addApplication =
    addOperatorApplication 1 2


addOperatorApplication : Int -> Int -> Int
addOperatorApplication a b =
    a + b



-- MAIN


type Msg
    = AddOne Int
    | OnePlusOne
    | Answer
    | QuickBrownFox


main : Program () () Msg
main =
    let
        _ =
            -- required so that elm does not drop the code
            syntaxExamples
    in
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> () -> ( (), Cmd Msg )
update msg model =
    case msg of
        AddOne value ->
            ( model, outgoingAddOne (addOne value) )

        OnePlusOne ->
            ( model, outgoingOnePlusOne onePlusOne )

        Answer ->
            ( model, outgoingAnswer Helper.answer )

        QuickBrownFox ->
            ( model, outgoingQuickBrownFox NestedSomething.quickBrownFox )


subscriptions : () -> Sub Msg
subscriptions model =
    Sub.batch
        [ incomingAddOne AddOne
        , incomingOnePlusOne (\_ -> OnePlusOne)
        , incomingAnswer (\_ -> Answer)
        , incomingQuickBrownFox (\_ -> QuickBrownFox)
        ]
