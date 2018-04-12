module Types ( Spot(..), Piece, Board, Progress(..)) where

import Data.Set 

data Spot
  = Location (Int, Int)
  | Name Char
  deriving (Show, Eq, Ord)

type Piece = Set Spot

type Board = Set Spot

data Progress = Progress { used :: [Piece]
                     , uncovered :: Board
                     , unused :: Set Piece
                     } deriving (Eq, Show)
