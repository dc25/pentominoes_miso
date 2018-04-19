module Init ( initialProgress ) where

import Control.Monad
import Data.List 
import Data.Set as DS

import Piece
import Solve

type Board = Set Element

-- all the placements of a piece on a board ; may be duplicates
positions0 :: Board -> Piece -> [Piece]
positions0 board p = do
  pv <- variants p
  let ((minRow, minCol), (maxRow, maxCol)) = bounds pv
      ((minRowBoard, minColBoard), (maxRowBoard, maxColBoard)) = bounds board
  vt <- [minRowBoard - maxRow .. maxRowBoard - minRow]
  ht <- [minColBoard - maxCol .. maxColBoard - minCol]
  let translated = translate (vt,ht) pv 
  guard $ translated `isSubsetOf` board
  return $ translated

-- all the placements of a piece on a board ; no duplicates
positions :: Board -> Piece -> [Piece]
positions board p = nub $ positions0 board p

-- Given an image (list of lists) of a many piece names (chars) and 
-- the name of a Piece, construct that Piece (a Set of Spots) .
nameToPiece :: [[Char]] -> Char -> Piece
nameToPiece image name = 
  let unboundedGrid = [[(row, col) | row <- [0 .. ]] | col <- [0 .. ]]
      indexedGrid = concat $ zipWith zip unboundedGrid image
      pieceCoords = Prelude.filter (\((r, c), n) -> n == name) indexedGrid
  in fromList $ Name name : ((Location . fst) <$> pieceCoords)

initialProgress :: [(Int, Int)] -> [[Char]] -> Progress Element
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
