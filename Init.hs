module Init ( initialProgress ) where

import Control.Monad
import Data.List 
import Data.Set as DS

import Puzzle
import Solve

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

-- rotations and flipped rotations of a piece.
variants :: Piece -> [Piece]
variants p0 =
  let p1 = rotatePiece p0
      p2 = rotatePiece p1
      p3 = rotatePiece p2
      r0 = flipPiece p0
      r1 = flipPiece p1
      r2 = flipPiece p2
      r3 = flipPiece p3
  in [p0,p1,p2,p3,r0,r1,r2,r3]

-- all the placements of a piece on a board ; may be duplicates
positions0 :: Board -> Piece -> [Piece]
positions0 board p = do
  pv <- variants p
  let ((minRow, minCol), (maxRow, maxCol)) = bounds pv
      ((minRowBoard, minColBoard), (maxRowBoard, maxColBoard)) = bounds board
  vt <- [minRowBoard - maxRow .. maxRowBoard - minRow]
  ht <- [minColBoard - maxCol .. maxColBoard - minCol]
  let translated = translatePiece pv (vt,ht)
  guard $ translated `isSubsetOf` board
  return $ translatePiece pv (vt, ht)

-- all the placements of a piece on a board ; no duplicates
positions :: Board -> Piece -> [Piece]
positions board p = nub $ positions0 board p

-- Given an image (list of lists) of a bunch of piece names (chars) and 
-- the name of a particular piece, construct the Piece (a Set of Spots) .
nameToPiece :: [[Char]] -> Char -> Piece
nameToPiece image name = 
  let unboundedGrid = [[(row, col) | row <- [0 .. ]] | col <- [0 .. ]]
      indexedGrid = concat $ zipWith zip unboundedGrid image
      pieceCoords = Prelude.filter (\((r, c), n) -> n == name) indexedGrid
  in fromList $ Name name : ((Location . fst) <$> pieceCoords)

initialProgress :: [(Int, Int)] -> [[Char]] -> Progress Spot
initialProgress squares image = 
  let -- gather the names
      names = nub $ concat image

      -- construct the board - a set of both Name and Location Spots.
      emptyBoard = fromList $ fmap Name names ++ fmap Location squares

      -- construct the pieces by name.
      pieces = fmap (nameToPiece image) names

      -- gather all possible placements of the pieces on the board.
      placements = fromList $ concat $ (positions emptyBoard) <$> pieces
  in Progress [] emptyBoard placements
