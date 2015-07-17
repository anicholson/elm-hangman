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

import Keyboard
import Char

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

lettersMatch : (Letter, GuessedLetter) -> Bool
lettersMatch pair =
    case snd pair of
        Nothing -> False
        Just l  -> l == fst pair

correctGuess : Model -> Letter -> Bool
correctGuess model guess =
    let letters = String.toList model.word in
    List.member guess letters

haveGuessedLetter : List Letter -> Letter -> Bool
haveGuessedLetter guesses letter =
    List.member letter guesses

alreadyGuessed : List Letter -> Letter -> List Letter
alreadyGuessed guesses newGuess =
    let iGuess = Char.toUpper newGuess in
    let notaLetter = \c -> Char.isDigit c in
    if | List.member iGuess guesses -> guesses
       | notaLetter iGuess          -> guesses
       | otherwise                  -> iGuess :: guesses

checkWord : Model -> List GuessedLetter
checkWord model =
  checkLetters (String.toList model.word) model.guesses

checkLetters : List Letter -> List Letter  -> List GuessedLetter
checkLetters word guesses  =
    let currentLetter        = List.head word in
      let guessedCurrentLetter = case currentLetter of
                                   Nothing -> False
                                   Just l  -> List.member l guesses
          result               = if guessedCurrentLetter then currentLetter else Nothing
    in
        case (List.tail word)  of
          Nothing   -> [currentLetter]
          Just rest -> (result) :: (checkLetters rest guesses)

updateCorrectGuesses : Model -> Model
updateCorrectGuesses model =
    { model | correctGuesses <- checkWord model }

resolveGuess : Letter  -> Model -> Model
resolveGuess letter model =
    let updateGuesses    =  \model -> { model | guesses    <- alreadyGuessed model.guesses letter }
        updateGuessCount =  \model -> { model | guessCount <- List.length model.guesses           }
    in
      model |> updateGuesses
            |> updateGuessCount
            |> updateCorrectGuesses

resolveModel : Model -> Model
resolveModel model =
    let wordGuessPairs   = zip (String.toList model.word)  model.correctGuesses
        correctlyGuessed = List.all lettersMatch wordGuessPairs
        newGameState     = if correctlyGuessed then Won else Playing
    in { model | gameStatus <- newGameState }



{-| Updates the state of a Model given an action taken -}
update : Action -> Model -> Model
update action model =
    resolveModel <| case action of
      Reset        -> initialModel "ELEPHANT"
      Guess letter -> resolveGuess letter model

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

guessedLetter = Signal.map (Char.fromCode >> Guess) Keyboard.presses

model : Signal Model
model =
  let updates = Signal.merge guessedLetter actions.signal
  in Signal.foldp update (initialModel "ELEPHANT")  updates

actions : Signal.Mailbox Action
actions =
    Signal.mailbox Reset

{-| Bootstrap the app! -}
main : Signal Html
main =
    Signal.map (view actions.address) model
