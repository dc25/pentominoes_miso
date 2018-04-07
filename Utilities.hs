module Utilities ( isLocation, isName, getName, getLocation, getLocations) where

import Data.Set as DS

import Types

isLocation :: Place -> Bool
isLocation pl =
  case pl of
    Location coords -> True
    Name _ -> False

isName :: Place -> Bool
isName = not . isLocation

getName :: Piece -> Char
getName p =
  let Name ch = head $ toList $ DS.filter isName p
  in ch

getLocation :: Place -> (Int, Int)
getLocation pl =
  let Location lo = pl
  in lo

getLocations :: Piece -> [(Int, Int)]
getLocations = fmap getLocation . toList . DS.filter isLocation
