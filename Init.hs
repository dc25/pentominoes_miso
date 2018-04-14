module Init ( initialProgress ) where

import Data.List 
import Data.Set as DS

import Types
import Solve
import Utilities

translateSpot :: (Int, Int) -> Spot -> Spot
translateSpot (vshift, hshift) loc =
  case loc of
    Location (row, col) -> Location (row + vshift, col + hshift)
    _ -> loc

rotateSpot :: Spot -> Spot
rotateSpot loc =
  case loc of
    Location (row, col) -> Location (col, -row)
    _ -> loc

flipSpot :: Spot -> Spot
flipSpot loc =
  case loc of
    Location (row, col) -> Location (col, row)
    _ -> loc

translatePiece :: Piece -> (Int, Int) -> Piece
translatePiece p shift = DS.map (translateSpot shift) p

rotatePiece :: Piece -> Piece
rotatePiece p = DS.map rotateSpot p

flipPiece :: Piece -> Piece
flipPiece p = DS.map flipSpot p

translations :: Board -> Piece -> [(Int, Int)]
translations board p = do
  let ((minRow, minCol), (maxRow, maxCol)) = bounds p
      ((minRowBoard, minColBoard), (maxRowBoard, maxColBoard)) = bounds board
  vt <- [minRowBoard - maxRow .. maxRowBoard - minRow]
  ht <- [minColBoard - maxCol .. maxColBoard - minCol]
  return (vt, ht)

placements :: Board -> Piece -> [Piece]
placements board p =
  Prelude.filter
    (`isSubsetOf` board)
    (fmap (translatePiece p) (translations board p))

fullPlacements :: Board -> Piece -> [Piece]
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

-- Given a list of pieces and a board (Set of Spots) construct the
-- set of all placements on the board of translations and rotations 
-- of the pieces, 
allPlacements :: Board -> [Piece] -> Set Piece
allPlacements board pieces = fromList $ concatMap (fullPlacements board) pieces

-- Given an image (list of lists) of a bunch of piece names (chars) and 
-- the name of a particular piece, construct the Piece (a Set of Spots) .
nameToPiece :: [[Char]] -> Char -> Piece
nameToPiece image name = 
  let unboundedGrid = [[(row, col) | row <- [0 .. ]] | col <- [0 .. ]]
      indexed = concat $ zipWith zip unboundedGrid image
      pieceCoords = Prelude.filter (\((r, c), n) -> n == name) indexed
  in fromList $ Name name : ((Location . fst) <$> pieceCoords)

initialProgress :: [(Int, Int)] -> [[Char]] -> Progress Spot
initialProgress squares image = 
  let -- gather the names
      names = nub $ concat image

      -- construct the pieces by name.
      pieces = fmap (nameToPiece image) names

      -- construct the board - a set of both Name and Location Spots.
      emptyBoard = fromList $ fmap Name names ++ fmap Location squares

      -- gather all possible placements of the pieces on the board.
      placements = allPlacements emptyBoard pieces 
  in Progress [] emptyBoard placements

