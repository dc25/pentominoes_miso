module Types ( Place(..), Piece, Board, Progress(..), History ) where

import Data.Set 

data Place
  = Location (Int, Int)
  | Name Char
  deriving (Show, Eq, Ord)

type Piece = Set Place

type Board = Set Place

data Progress = Progress { used :: [Piece]
                     , uncovered :: Board
                     , unused :: Set Piece
                     } deriving (Eq, Show)

type History = [[Progress]]

