module Piece (Cell(..), Piece, getName, getLocations) where

import Data.Set as DS

data Cell
  = Location (Int, Int)
  | Name Char
  deriving (Show, Eq, Ord)

type Piece = Set Cell

isLocation :: Cell -> Bool
isLocation (Location _) = True
isLocation (Name _) = False

isName :: Cell -> Bool
isName = not . isLocation

getName :: Piece -> Char
getName p =
  let Name ch = head $ toList $ DS.filter isName p
  in ch

getLocation :: Cell -> (Int, Int)
getLocation (Location lo) = lo

getLocations :: Piece -> [(Int, Int)]
getLocations = fmap getLocation . toList . DS.filter isLocation

