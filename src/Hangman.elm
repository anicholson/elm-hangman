module Hangman (Model, Letter, initialModel, Action, update, view, main) where

{-| A hangman game in Elm.

@docs Model
@docs Letter
@docs Action
@docs initialModel
@docs update
@docs view
@docs main
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events     exposing (..)
import Html.Lazy       exposing (lazy, lazy2, lazy3)
import Json.Decode as Json

import String exposing (toList)
import Signal          exposing (Signal, Address)
import Window


---- MODEL ----
{-|-}
type GameStatus = Won | Lost | Playing

{-|-}
type alias GuessedLetter = Maybe Letter

{-|-}
type alias Letter = Char

{-| Model for the game.

  word           : The word to guess.
  correctGuesses : If a letter's been guessed, show it, otherwise Nothing
  guessCount     : Number of Guesses made
  guesses        : All the letters that have been guessed
  gameStatus     : Status of the game.

-}

type alias Model = {
      word           : String
    , correctGuesses : List GuessedLetter
    , guessCount     : Int
    , guesses        : List Letter
    , guessed        : Bool
    , gameStatus     : GameStatus
    }

{-| Create an instance of a Model.
  @param wordToGuess: String = the word the player has to guess.
-}
initialModel : String -> Model
initialModel wordToGuess =
    {
      word           = wordToGuess
    , correctGuesses = List.repeat (String.length wordToGuess) Nothing
    , guessCount     = 0
    , guesses        = []
    , guessed        = False
    , gameStatus     = Playing }

---- UPDATE ----

-- A description of the kinds of actions that can be performed on the model of
-- our application.

zip : List a -> List b -> List (a,b)
zip = List.map2 (,)

{-| The kinds of action that can be taken. Guess a letter, or reset the game -}
type Action
    = Guess Letter
    | Reset

resolveGuess : Letter  -> Model -> Model
resolveGuess letter model =
    model

resolveModel : Model -> Model
resolveModel model =
    let correctlyGuessed = (\model -> zip (String.toList model.word)  model.correctGuesses)
    in model


{-| Updates the state of a Model given an action taken -}
update : Action -> Model -> Model
update action model =
    resolveModel <| case action of
      Reset -> initialModel "elephant"
      Guess letter ->
          { model |
            guesses <- letter :: model.guesses
          , guessCount <- model.guessCount + 1
          }

guessList : List Letter -> String
guessList guesses =
    let spaced = List.intersperse  ' '  guesses
    in
      String.fromList spaced

wordInProgress : List GuessedLetter ->  Html.Html
wordInProgress letters =
    let guessToChar = \guess ->
                      case guess of
                        Just l   -> l
                        Nothing  -> '_'
        letterToLi = \letter -> li [] [text (toString letter)]
    in
      ul [class "word-space"] (List.map (letterToLi << guessToChar) letters)

{-| Takes Signalled Actions and a Model and keeps the view of that model in sync. -}

view : Signal.Address Action -> Model -> Html.Html
view address model =
    div [id "hangman"] [
             div [id "word"] [(wordInProgress model.correctGuesses)]
            , div [id "guessing"] [
                       p [] [
                        text "Guessed letters: "
                       , text (guessList model.guesses)],
                       p [] [
                        text "# guesses: "
                       , text (toString model.guessCount) ]]]


-- manage the model of our application over time
model : Signal Model
model = Signal.foldp update (initialModel "elephant")  actions.signal

actions : Signal.Mailbox Action
actions =
    Signal.mailbox Reset

{-| Bootstrap the app! -}
main : Signal Html
main =
    Signal.map (view actions.address) model
