module Main where

import Game
import Graphics.Gloss
import Render
import Transform

window :: Display
window = InWindow "Tic-tac-toe" (screenW, screenH) (50, 50)

main :: IO ()
main = play window white 30 newGame gameAsPicture transformGame (const id)
