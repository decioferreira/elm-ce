port module Main exposing (main)

-- INCOMING MESSAGES


port incomingAddOne : (Int -> msg) -> Sub msg


port incomingOnePlusOne : (() -> msg) -> Sub msg



-- OUTGOING MESSAGES


port outgoingAddOne : Int -> Cmd msg


port outgoingOnePlusOne : Int -> Cmd msg



-- FUNCTIONALITY


addOne : Int -> Int
addOne a =
    1 + a


onePlusOne : Int
onePlusOne =
    1 + 1



-- MAIN


type Msg
    = AddOne Int
    | OnePlusOne


main : Program () () Msg
main =
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


subscriptions : () -> Sub Msg
subscriptions model =
    Sub.batch
        [ incomingAddOne AddOne
        , incomingOnePlusOne (always OnePlusOne)
        ]
