module App exposing (main)

import Char
import Game exposing (Msg(..))
import Browser
import Browser.Events
import Views exposing (view)
import Json.Decode as Decode


-- manage the model of our application over time


guessedLetterDecoder : Decode.Decoder Msg
guessedLetterDecoder =
    Decode.map toMsg (Decode.field "key" Decode.string)


toMsg : String -> Msg
toMsg string =
    case String.uncons string of
        Just (char, "") ->
            if Char.isLower char then
                Guess char
            else
                NoOp
        _ ->
            NoOp 


subscriptions : Game.Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyPress guessedLetterDecoder
        ]


{-| Bootstrap the app!
-}
main : Program Int Game.Model Msg
main =
    Browser.element
        { init = Game.initialModel
        , view = view
        , update = Game.update
        , subscriptions = subscriptions
        }
