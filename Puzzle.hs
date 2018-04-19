module Puzzle (Spot(..), Piece, getName, getLocations, bounds, translate, variants) where

import Data.Set as DS

data Spot
  = Location (Int, Int)
  | Name Char
  deriving (Show, Eq, Ord)

type Piece = Set Spot

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

rotate :: Piece -> Piece
rotate = DS.map rotateSpot 
  where
    rotateSpot loc =
      case loc of
        Location (row, col) -> Location (col, -row)
        _ -> loc

reflect :: Piece -> Piece
reflect = 
  DS.map reflectSpot 
  where
    reflectSpot loc =
      case loc of
        Location (row, col) -> Location (col, row)
        _ -> loc

translate :: (Int, Int) -> Piece -> Piece
translate shift = 
  DS.map (translateSpot shift) 
  where
    translateSpot (vshift, hshift) loc =
      case loc of
        Location (row, col) -> Location (row + vshift, col + hshift)
        _ -> loc

-- rotations and reflected rotations of a piece.
variants :: Piece -> [Piece]
variants p0 =
  let p1 = rotate p0
      p2 = rotate p1
      p3 = rotate p2
      r0 = reflect p0
      r1 = reflect p1
      r2 = reflect p2
      r3 = reflect p3
  in [p0,p1,p2,p3,r0,r1,r2,r3]
