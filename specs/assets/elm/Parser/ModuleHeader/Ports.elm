port module Ports exposing (messageReceiver, sendMessage)


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg
