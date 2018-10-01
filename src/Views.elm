module Views exposing (guessList, guessToChar, lostView, playAgainButton, playingView, view, wonView, wordInProgress)

import Game exposing (GameStatus(..), GuessedLetter(..), Letter, Model, Msg)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String


guessList : List Letter -> String
guessList guesses =
    let
        spaced =
            List.intersperse ' ' guesses
    in
    String.fromList spaced


guessToChar : GuessedLetter -> Char
guessToChar guess =
    case guess of
        Guessed l ->
            l

        Unguessed ->
            '_'


wordInProgress : List GuessedLetter -> Html Msg
wordInProgress letters =
    let
        letterToLi =
            \letter -> li [] [ text (String.fromChar letter) ]
    in
    ul [ class "word-space" ] (List.map (letterToLi << guessToChar) letters)


playAgainButton : Html Msg
playAgainButton =
    button [ onClick Game.Reset ] [ text "Play Again!" ]


lostView : Model -> Html Msg
lostView model =
    div [ id "hangman" ]
        [ header [ id "condolence" ] [ h1 [] [ text "Bad luck!" ] ]
        , p [ class "message" ] [ text "You ran out of guesses. The word to guess was:" ]
        , p [ class "theActualWord" ] [ text model.word ]
        , playAgainButton
        ]


wonView : Model -> Html Msg
wonView model =
    div [ id "hangman" ]
        [ header [ id "congratulation" ] [ h1 [] [ text "Congratulations!" ] ]
        , p [ class "message" ] [ text <| "You guessed the word with " ++ String.fromInt model.guessesLeft ++ " guesses remaining. Well done!" ]
        , playAgainButton
        ]


playingView : Model -> Html Msg
playingView model =
    div [ id "hangman" ]
        [ div [ id "word" ] [ wordInProgress model.correctGuesses ]
        , div [ id "guessing" ]
            [ p []
                [ text "Guessed letters: "
                , text (guessList model.guesses)
                ]
            , p []
                [ text "Incorrect guesses remaining: "
                , text (String.fromInt model.guessesLeft)
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    let
        newView =
            case model.gameStatus of
                Won ->
                    wonView

                Lost ->
                    lostView

                Playing ->
                    playingView

                otherwise ->
                    \m -> text "not Playing"
    in
    newView model
