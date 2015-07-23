module Views where

import Game exposing (Action, Letter, GuessedLetter, Model)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events     exposing (..)
import Html.Lazy       exposing (lazy, lazy2, lazy3)
import String
import Json.Decode as Json
import Keyboard

guessList : List Letter -> String
guessList guesses =
    let spaced = List.intersperse  ' '  guesses
    in
      String.fromList spaced

guessToChar : GuessedLetter -> Char
guessToChar guess =
    case guess of
      Guessed l -> l
      Unguessed -> '_'

wordInProgress : List GuessedLetter ->  Html.Html
wordInProgress letters =
    let
        letterToLi = \letter -> li [] [text (String.fromChar letter)]
    in
      ul [class "word-space"] (List.map (letterToLi << guessToChar) letters)

{-| Takes Signalled Actions and a Model and keeps the view of that model in sync. -}

view : Signal.Address Action -> Model -> Html.Html
view address model =
    case model.gameStatus of
      otherwise ->    div [id "hangman"] [
                           div [id "word"] [(wordInProgress model.correctGuesses)]
                          , div [id "guessing"] [
                                     p [] [
                                      text "Guessed letters: "
                                     , text (guessList model.guesses)],
                                     p [] [
                                      text "Incorrect guesses remaining: "
                                     , text (toString model.guessCount) ]]]
