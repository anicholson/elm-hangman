module App exposing (..)

import Game exposing (Msg(NoOp, Guess, NewGame))
import Views exposing (view)

import Char
import Keyboard
import Html

-- manage the model of our application over time

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
       ]

{-| Bootstrap the app! -}
main : Program Int Game.Model Msg
main =
  Html.programWithFlags
    { init = Game.initialModel
    , view = view
    , update = Game.update
    , subscriptions = subscriptions
    }
