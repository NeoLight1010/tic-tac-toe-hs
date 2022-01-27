module Game where

import Data.Array

data Player = X | O deriving (Eq, Show)
data State = Playing | GameOver (Maybe Player) deriving Show

data Cell = Empty | Full Player deriving Show
type Board = Array (Int, Int) Cell

data Game = Game {
    board :: Board,
    turn :: Player,
    state :: State
} deriving Show

n :: Int
n = 3

newGame = Game {
    board = array indexRange $ zip (range indexRange) (repeat Empty),
    turn = X,
    state = Playing
} where indexRange = ((0, 0), (n - 1, n - 1))
