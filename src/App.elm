port module App exposing (..)

import Game exposing (Msg(Guess, Reset))
import Views exposing (view)

import Char
import Keyboard
import Html.App as Application
import Json.Decode exposing (int)

-- manage the model of our application over time

port initialSeed : (Int -> msg) -> Sub msg

guessedLetter : Keyboard.KeyCode -> Msg
guessedLetter code = Guess (Char.fromCode code)

subscriptions model =
  Sub.batch [
         Keyboard.presses guessedLetter
       , initialSeed Reset
       ]

{-| Bootstrap the app! -}
main =
  Application.program
    { init = (Game.initialModel, Cmd.none)
    , view = view
    , update = Game.update
    , subscriptions = subscriptions
    }
