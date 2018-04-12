module Init ( initialProgress ) where

import Data.List 
import Data.Set as DS

import Types
import Utilities

bounds :: Piece -> ((Int, Int), (Int, Int))
bounds p =
  let locations = DS.filter isLocation p
      coords = (\(Location (row, col)) -> (row, col)) <$> toList locations
      rows = fmap fst coords
      cols = fmap snd coords
   in ((minimum rows, minimum cols), (maximum rows, maximum cols))

translateLocation :: (Int, Int) -> Spot -> Spot
translateLocation (vshift, hshift) loc =
  case loc of
    Location (row, col) -> Location (row + vshift, col + hshift)
    _ -> loc

rotateLocation :: Spot -> Spot
rotateLocation loc =
  case loc of
    Location (row, col) -> Location (col, -row)
    _ -> loc

flipLocation :: Spot -> Spot
flipLocation loc =
  case loc of
    Location (row, col) -> Location (col, row)
    _ -> loc

translatePiece :: Piece -> (Int, Int) -> Piece
translatePiece p shift = DS.map (translateLocation shift) p

rotatePiece :: Piece -> Piece
rotatePiece p = DS.map rotateLocation p

flipPiece :: Piece -> Piece
flipPiece p = DS.map flipLocation p

translations :: Set Spot -> Piece -> [(Int, Int)]
translations board p = do
  let ((minRow, minCol), (maxRow, maxCol)) = bounds p
      ((minRowBoard, minColBoard), (maxRowBoard, maxColBoard)) = bounds board
  vt <- [minRowBoard - maxRow .. maxRowBoard - minRow]
  ht <- [minColBoard - maxCol .. maxColBoard - minCol]
  return (vt, ht)

placements board p =
  Prelude.filter
    (`isSubsetOf` board)
    (fmap (translatePiece p) (translations board p))

fullPlacements board p0 =
  let p1 = rotatePiece p0
      p2 = rotatePiece p1
      p3 = rotatePiece p2
      r0 = flipPiece p0
      r1 = flipPiece p1
      r2 = flipPiece p2
      r3 = flipPiece p3

      placementsWithDuplicates =
           placements board p0 
        ++ placements board p1 
        ++ placements board p2 
        ++ placements board p3 
        ++ placements board r0 
        ++ placements board r1 
        ++ placements board r2 
        ++ placements board r3

   in nub placementsWithDuplicates

allPlacements :: [Piece] -> Set Spot -> Set Piece
allPlacements pieces board = fromList $ concatMap (fullPlacements board) pieces

nameToPiece :: [[Char]] -> Char -> Piece
nameToPiece image name = 
  let unboundedGrid = [[(row, col) | row <- [0 .. ]] | col <- [0 .. ]]
      indexed = concat $ zipWith zip unboundedGrid image
      pieceCoords = Prelude.filter (\((r, c), n) -> n == name) indexed
  in fromList $ Name name : ((Location . fst) <$> pieceCoords)

initialProgress :: [(Int, Int)] -> [[Char]] -> Progress
initialProgress squares image = 
  let names = nub $ concat image

      pieces = fmap (nameToPiece image) names

      board = fromList $ fmap Name names ++ fmap Location squares
      placements = allPlacements pieces board
  in Progress [] board placements

