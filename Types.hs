module Types (Spot(..), Piece, Board) where

import Data.Set 

data Spot
  = Location (Int, Int)
  | Name Char
  deriving (Show, Eq, Ord)

type Piece = Set Spot

type Board = Set Spot

