module Main exposing (main, onePlusOne)


addOne : Int -> Int
addOne a =
    1 + a


onePlusOne : Int
onePlusOne =
    1 + 1


main : Program () () msg
main =
    let
        _ =
            addOne

        _ =
            onePlusOne
    in
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
