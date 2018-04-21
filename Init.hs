module Init ( initialProgress ) where

import Control.Monad
import Data.List 
import Data.Set as DS

import Piece
import Solve

type Board = Set Element

-- all the placements of a piece on a board ; may be duplicates
positions0 :: [(Int,Int)] -> [(Int,Int)]-> [[(Int,Int)]]
positions0 board p = do
  pv <- variants p
  let ((minRow, minCol), (maxRow, maxCol)) = bounds pv
      ((minRowBoard, minColBoard), (maxRowBoard, maxColBoard)) = bounds board
  vt <- [minRowBoard - maxRow .. maxRowBoard - minRow]
  ht <- [minColBoard - maxCol .. maxColBoard - minCol]
  let translated = translate (vt,ht) pv 
  guard $ fromList translated `isSubsetOf` fromList board
  return $ translated

-- all the placements of a piece on a board ; no duplicates
positions :: [(Int,Int)] -> [(Int,Int)]-> [[(Int,Int)]]
positions board p = nub $ positions0 board p

nameToPlacements :: [(Int, Int)] -> [[Char]] -> Char -> [Piece]
nameToPlacements squares image name =
  let unboundedGrid = [[(row, col) | row <- [0 .. ]] | col <- [0 .. ]]
      indexedGrid = concat $ zipWith zip unboundedGrid image
      hasName ((row,col), n) = n == name
      pieceCoords = fmap fst $ Prelude.filter hasName indexedGrid
  in fromList . (Name name :) . fmap Location <$> positions squares pieceCoords

initialProgress :: [(Int, Int)] -> [[Char]] -> Progress Element
initialProgress squares image = 
  let -- gather the names
      names = nub $ concat image
      emptyBoard = fromList $ fmap Name names ++ fmap Location squares
      placements = fromList $ concatMap (nameToPlacements squares image) names
  in Progress [] emptyBoard placements
