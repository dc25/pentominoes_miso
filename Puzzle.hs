module Puzzle (Spot(..), Piece, Board, isLocation, isName, getName, getLocation, getLocations, bounds) where

import Data.Set as DS

data Spot
  = Location (Int, Int)
  | Name Char
  deriving (Show, Eq, Ord)

type Piece = Set Spot

type Board = Set Spot

isLocation :: Spot -> Bool
isLocation (Location _) = True
isLocation (Name _) = False

isName :: Spot -> Bool
isName = not . isLocation

getName :: Piece -> Char
getName p =
  let Name ch = head $ toList $ DS.filter isName p
  in ch

getLocation :: Spot -> (Int, Int)
getLocation (Location lo) = lo

getLocations :: Piece -> [(Int, Int)]
getLocations = fmap getLocation . toList . DS.filter isLocation

bounds :: Set Spot -> ((Int, Int), (Int, Int))
bounds p =
  let locations = DS.filter isLocation p
      coords = (\(Location (row, col)) -> (row, col)) <$> toList locations
      rows = fmap fst coords
      cols = fmap snd coords
   in ((minimum rows, minimum cols), (maximum rows, maximum cols))

