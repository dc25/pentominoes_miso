module Piece (Element(..), Piece, getName, getLocations) where

import Data.Set as DS

data Element
  = Location (Int, Int)
  | Name Char
  deriving (Show, Eq, Ord)

type Piece = Set Element

isLocation :: Element -> Bool
isLocation (Location _) = True
isLocation (Name _) = False

isName :: Element -> Bool
isName = not . isLocation

getName :: Piece -> Char
getName p =
  let Name ch = head $ toList $ DS.filter isName p
  in ch

getLocation :: Element -> (Int, Int)
getLocation (Location lo) = lo

getLocations :: Piece -> [(Int, Int)]
getLocations = fmap getLocation . toList . DS.filter isLocation

