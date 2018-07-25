# Arrowized Tic-Tac-Toe in Haskell
The traditional tic tac toe game with core logic written in Haskell's Control.Arrow interface and an arrow instance from https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial.

# To play
The game is built with the Haskell Tool Stack. You can get it by installing the Haskell platform: https://www.haskell.org/platform/

On Unix systems, make sure you have AmericanTypewritter.ttc font in /Library/Fonts/ (or just go to app/Main.hs to edit the filepath and use whatever other font you like).
Clone the repo and then run: 

stack build

stack exec higlut-exe
