module Render (gameAsPicture, screenW, screenH) where

import Game
import Graphics.Gloss
import Data.Array

screenW :: Int
screenW = 640

screenH :: Int
screenH = 480

cellH :: Float
cellH = fromIntegral screenH / fromIntegral n

cellW :: Float
cellW = fromIntegral screenW / fromIntegral n

--

pXColor :: Color
pXColor = red

pOColor :: Color
pOColor = blue

tieColor :: Color
tieColor = greyN 0.5

--

pXPicture :: Picture
pXPicture = blank

pOPicture :: Picture
pOPicture = blank

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
boardAsPicture b =
  pictures
    [ xCellsOfBoardAsPicture b,
      oCellsOfBoardAsPicture b,
      gridPicture
    ]

xCellsOfBoardAsPicture :: Board -> Picture
xCellsOfBoardAsPicture = cellsOfBoardPicture (Full X) pXPicture

oCellsOfBoardAsPicture :: Board -> Picture
oCellsOfBoardAsPicture = cellsOfBoardPicture (Full O) pOPicture

gridPicture :: Picture
gridPicture = Blank

-- | Takes a Cell and its Picture representation, and returns a Picture with all the cells correctly placed.
cellsOfBoardPicture :: Cell -> Picture -> Board -> Picture
cellsOfBoardPicture c p b =
    pictures
    $ map (snapPictureToCell p . fst)
    $ filter ((== c) . snd)
    $ assocs b

snapPictureToCell :: Picture -> (Int, Int) -> Picture
snapPictureToCell p (row, column) = translate x y p
    where x = fromIntegral column * cellW + cellW * 0.5
          y = fromIntegral row * cellH + cellH * 0.5
