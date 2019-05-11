port module Main exposing (main)


port convert : (String -> msg) -> Sub msg


port converted : String -> Cmd msg


type alias Model =
    ()


main : Program () Model Msg
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = update
        , subscriptions = always subscriptions
        }


type Msg
    = Convert String


update : Msg -> () -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Convert content ->
            let
                js =
                    """
                    this.onePlusOne = 2;
                    this.addOne = function(a) { return 1 + a; };
                    """
            in
            ( model, converted js )


subscriptions : Sub Msg
subscriptions =
    convert Convert
