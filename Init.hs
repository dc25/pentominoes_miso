module Init ( initialProgress, bounds ) where

import Control.Monad
import Data.List 
import Data.Set as DS

import Piece
import Solve

type Board = Set Cell

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

bounds :: [(Int,Int)] -> ((Int, Int), (Int, Int))
bounds coords =
  let rows = fmap fst coords
      cols = fmap snd coords
  in ((minimum rows, minimum cols), (maximum rows, maximum cols))

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

initialProgress :: [(Int, Int)] -> [[Char]] -> Progress Cell
initialProgress squares image = 
  let -- gather the names
      names = nub $ concat image
      emptyBoard = fromList $ fmap Name names ++ fmap Location squares
      placements = fromList $ concatMap (nameToPlacements squares image) names
  in Progress [] emptyBoard placements
