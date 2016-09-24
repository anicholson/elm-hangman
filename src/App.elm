port module App exposing (..)

import Game exposing (Msg(NoOp, Guess, NewGame))
import Views exposing (view)

import Char
import Keyboard
import Html.App as Application
import Json.Decode exposing (int)

-- manage the model of our application over time

port initialSeed : (Int -> msg) -> Sub msg

guessedLetter : Keyboard.KeyCode -> Msg
guessedLetter code =
  let char = Char.toLower <| Char.fromCode code
      valid = Char.isLower char
  in
    if valid then
      Guess char
    else
      NoOp

subscriptions : Game.Model -> Sub Msg
subscriptions model =
  Sub.batch [
         Keyboard.presses guessedLetter
       , initialSeed NewGame
       ]

{-| Bootstrap the app! -}
main =
  Application.programWithFlags
    { init = Game.initialModel
    , view = view
    , update = Game.update
    , subscriptions = subscriptions
    }
