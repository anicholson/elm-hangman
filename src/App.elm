module App where

import Game
import Views exposing (view)



-- manage the model of our application over time

guessedLetter = Signal.map (Char.fromCode >> Guess) Keyboard.presses

model : Signal Game.Model
model =
  let updates = Signal.merge guessedLetter actions.signal
  in Signal.foldp update (Game.initialModel "ELEPHANT")  updates

actions : Signal.Mailbox Game.Action
actions =
    Signal.mailbox Reset

{-| Bootstrap the app! -}
main : Signal Html
main =
    Signal.map (view actions.address) model
