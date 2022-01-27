module Render (gameAsPicture) where

import Graphics.Gloss
import Game

pXColor :: Color
pXColor = red

pOColor :: Color
pOColor = blue

tieColor :: Color
tieColor = greyN 0.5

--

gameAsPicture :: Game -> Picture
gameAsPicture g = case state g of
  Playing -> boardAsPlayingPicture (board g)
  GameOver w -> boardAsGameOverPicture w (board g)

-- 

boardAsPlayingPicture :: Board -> Picture
boardAsPlayingPicture = const Blank

boardAsGameOverPicture :: Maybe Player -> Board -> Picture
boardAsGameOverPicture winner b = color (outcomeColor winner) (boardAsPicture b)

--

outcomeColor :: Maybe Player -> Color
outcomeColor mp = case mp of
  Just X -> pXColor
  Just O -> pOColor
  Nothing -> tieColor

boardAsPicture :: Board -> Picture
boardAsPicture b = pictures [
                            xCellsOfBoardAsPicture b
                          , oCellsOfBoardAsPicture b
                          , gridPicture
                          ]

xCellsOfBoardAsPicture :: Board -> Picture
xCellsOfBoardAsPicture = const blank

oCellsOfBoardAsPicture :: Board -> Picture
oCellsOfBoardAsPicture = const blank

gridPicture :: Picture
gridPicture = Blank
