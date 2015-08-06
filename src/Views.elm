module Views where

import Game exposing (Action, Letter, GuessedLetter(..), Model, GameStatus(..))

import Html exposing (..)
import Html.Attributes exposing (..)
import String

import Signal

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

lostView : Model -> Html.Html
lostView model = div [ id "hangman" ] [
                  header [ id "condolence" ] [ h1 [] [ text "Bad luck!" ]]
                 , p [ class "message" ] [ text "You ran out of guesses. The word to guess was:" ]
                 , p [ class "theActualWord" ] [ text model.word ]]


wonView : Model -> Html.Html
wonView model = div [ id "hangman" ] [
                 header [ id "congratulation" ] [ h1 [] [ text "Congratulations!" ]]
                , p [ class "message" ]  [ text <| "You guessed the word with " ++ toString(model.guessesLeft)  ++ " guesses remaining. Well done!" ]
                ]



playingView : Model -> Html.Html
playingView model =
   div [id "hangman"] [
            div [id "word"] [(wordInProgress model.correctGuesses)]
           , div [id "guessing"] [
                      p [] [
                       text "Guessed letters: "
                      , text (guessList model.guesses)],
                      p [] [
                       text "Incorrect guesses remaining: "
                      , text (toString model.guessesLeft) ]]]



view : Signal.Address Action -> Model -> Html.Html
view address model =
    let newView = case model.gameStatus of
                 Won       -> wonView

                 Lost      -> lostView

                 Playing   -> playingView

                 otherwise -> \m -> text "not Playing"
    in
      newView model
