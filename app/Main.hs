module Main where

import Game
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event)
import Render
import Transform

window :: Display
window = InWindow "Tic-tac-toe" (640, 480) (50, 50)

main :: IO ()
main = play window white 30 game gameAsPicture transformGame (const id)
