module Game (Model, Letter, GuessedLetter(Guessed, Unguessed), initialModel, Action(Guess, Reset, NoOp), GameStatus(Won, Lost, Playing), update) where

{-| A hangman game in Elm.

@docs Model
@docs Letter
@docs GuessedLetter
@docs Action
@docs initialModel
@docs update
@docs GameStatus
-}

import Char
import String
import Random
import Words


---- MODEL ----
{-|

The states the Game could be in at any given time.
None is when no game has been played yet.

-}
type GameStatus = Won
                | Lost
                | Playing
                | None

{-|

  Represents a letter in the player's view of the game. It may be guessed, in which case
  the value is known, or it may be Unguessed, when it it not.

-}
type GuessedLetter = Guessed Letter
                   | Unguessed

{-|-}
type alias Letter = Char

{-| Model for the game.

  word           : The word to guess.
  correctGuesses : If a letter's been guessed, show it, otherwise Nothing
  guessCount     : Number of incorrect guesses remaining before the game is Lost
  guesses        : All the letters that have been guessed
  gameStatus     : Status of the game.

-}


type alias Model = {
      word           : String
    , correctGuesses : List GuessedLetter
    , guessesLeft    : Int
    , guesses        : List Letter
    , gameStatus     : GameStatus
    , nextSeed       : Random.Seed
    }


{-| Create an instance of a Model.
  @param wordToGuess: String = the word the player has to guess.
-}
initialModel : Int -> Model
initialModel seed =
    let wordCount = Words.wordCount
        generator = Random.int 1 Words.wordCount
        initSeed = Random.initialSeed seed
        firstWordIndex = Random.generate generator initSeed
        wordToGuess = Words.getWord <| fst firstWordIndex
    in
    {
      word           = wordToGuess
    , correctGuesses = List.repeat (String.length wordToGuess) Unguessed
    , guessesLeft    = 6
    , guesses        = []
    , gameStatus     = Playing
    , nextSeed       = initSeed
    }


---- UPDATE ----

-- A description of the kinds of actions that can be performed on the model of
-- our application.

zip : List a -> List b -> List (a,b)
zip = List.map2 (,)


{-| The kinds of action that can be taken. Guess a letter, or reset the game -}
type Action  = Guess Letter
             | Reset
             | NoOp

lettersMatch : (Letter, GuessedLetter) -> Bool
lettersMatch (letter, guessedLetter) =
    case guessedLetter of
        Unguessed -> False

        Guessed  l  -> l == letter


correctGuess : Model -> Letter -> Bool
correctGuess model guess =
    let
      letters = String.toList model.word
    in
      List.member guess letters


haveGuessedLetter : List Letter -> Letter -> Bool
haveGuessedLetter guesses letter =
    List.member letter guesses


alreadyGuessed : List Letter -> Letter -> List Letter
alreadyGuessed guesses newGuess =
    let
        notaLetter = \c -> Char.isDigit c
    in
      if List.member newGuess guesses then guesses
      else if notaLetter newGuess     then guesses
      else                             newGuess :: guesses

checkWord : Model -> List GuessedLetter
checkWord model =
    let wordLength        = String.length model.word
        newCorrectGuesses = checkLetters (String.toList model.word) model.guesses
    in
      List.take wordLength newCorrectGuesses

checkLetters : List Letter -> List Letter  -> List GuessedLetter
checkLetters word guesses  =
    let
        currentLetter        = List.head word
    in
      let
          guessedCurrentLetter = case currentLetter of
                                   Nothing -> False

                                   Just l  -> List.member l guesses

          unTaggedLetter = case currentLetter of
                             Just l  -> l

                             Nothing -> ' '

          result = if guessedCurrentLetter then  Guessed unTaggedLetter else Unguessed
      in
        case (List.tail word)  of
          Nothing   -> [result]

          Just rest -> (result) :: (checkLetters rest guesses)


updateCorrectGuesses : Model -> Model
updateCorrectGuesses model =
    { model | correctGuesses = checkWord model }


updateGuessesLeft : Letter -> Model -> Model
updateGuessesLeft letter model =
    let alreadyGuessed = List.member letter model.guesses
        correctGuess   = List.member letter (String.toList model.word)
        same           = model.guessesLeft
        oneLess        = model.guessesLeft - 1
    in
      { model | guessesLeft = if (alreadyGuessed || correctGuess) then same else oneLess }


resolveGuess : Letter  -> Model -> Model
resolveGuess letter model =
    let
        iGuess         = Char.toUpper letter
        duplicateGuess =  List.member iGuess model.guesses
    in
      let
          updateGuesses    =  \model -> { model | guesses  = alreadyGuessed model.guesses iGuess }
      in
        model |> updateGuessesLeft iGuess
              |> updateGuesses
              |> updateCorrectGuesses


resolveModel : Model -> Model
resolveModel model =
    let wordGuessPairs   = zip (String.toList model.word)  model.correctGuesses
        correctlyGuessed = List.all lettersMatch wordGuessPairs
        noMoreGuesses    = model.guessesLeft == 0
        newGameState     = if correctlyGuessed then
                             Won
                           else if noMoreGuesses then
                             Lost
                           else 
                             Playing
    in { model | gameStatus = newGameState }


{-| Updates the state of a Model given an action taken -}
update : Action -> Model -> Model
update action model =
    resolveModel <| case action of
      Reset        -> initialModel 5
      Guess letter -> resolveGuess letter model
      otherwise    -> model
