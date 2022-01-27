module Render (gameAsPicture) where

import Graphics.Gloss
import Game

gameAsPicture :: Game -> Picture
gameAsPicture g = case state g of
  Playing -> boardAsPlayingPicture (board g)
  GameOver w -> boardAsGameOverPicture (state g) (board g)

boardAsPlayingPicture :: Board -> Picture
boardAsPlayingPicture = const Blank

boardAsGameOverPicture :: State -> Board -> Picture
boardAsGameOverPicture s = const Blank
