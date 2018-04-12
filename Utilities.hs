module Utilities ( isLocation, isName, getName, getLocation, getLocations) where

import Data.Set as DS

import Types

isLocation :: Place -> Bool
isLocation (Location _) = True
isLocation (Name _) = False

isName :: Place -> Bool
isName = not . isLocation

getName :: Piece -> Char
getName p =
  let Name ch = head $ toList $ DS.filter isName p
  in ch

getLocation :: Place -> (Int, Int)
getLocation (Location lo) = lo

getLocations :: Piece -> [(Int, Int)]
getLocations = fmap getLocation . toList . DS.filter isLocation
