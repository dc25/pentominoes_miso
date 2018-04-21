module Piece (Element(..), Piece, getName, getLocations, bounds, translate, variants) where

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

bounds :: [(Int,Int)] -> ((Int, Int), (Int, Int))
bounds coords =
  let rows = fmap fst coords
      cols = fmap snd coords
  in ((minimum rows, minimum cols), (maximum rows, maximum cols))

rotate :: [(Int,Int)] -> [(Int,Int)]
rotate = fmap (\(row,col) -> (col, -row)) 

reflect :: [(Int,Int)] -> [(Int,Int)]
reflect = fmap (\(row,col) -> (col, row)) 

translate :: (Int, Int) -> [(Int,Int)] -> [(Int,Int)]
translate (rowTrans, colTrans) = fmap (\(row,col) -> (row+rowTrans, col+colTrans)) 

-- rotations and reflected rotations of a piece.
variants :: [(Int,Int)] -> [[(Int,Int)]]
variants p0 =
  let p1 = rotate p0
      p2 = rotate p1
      p3 = rotate p2
      r0 = reflect p0
      r1 = reflect p1
      r2 = reflect p2
      r3 = reflect p3
  in [p0,p1,p2,p3,r0,r1,r2,r3]
