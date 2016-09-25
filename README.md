# elm-hangman
A hangman game, written in [Elm][elm].

# Running

## With Docker

- `docker build -t hangman .`
- `docker run -p 9999:80 -it hangman`
- Open `http://0.0.0.0:9999` in your browser :)x

## Without Docker

- `elm package install`
- `elm make src/App.elm --output dist/elm.js`
- open `dist/index.html` in your browser :)


[elm]: http://elm-lang.org