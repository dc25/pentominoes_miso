{-# LANGUAGE ScopedTypeVariables #-}

module Solve ( step0 , step , solve0) where

import Control.Monad.State
import qualified Data.List as DL (nub)
import Data.Set as DS

import Types
import Init

isLocation :: Place -> Bool
isLocation pl =
  case pl of
    Location coords -> True
    Name _ -> False

isName :: Place -> Bool
isName = not . isLocation

next :: Progress -> [Progress]
next (Progress used uncovered unused) = do
  guard (not $  DS.null uncovered) -- no space left (solved)

  -- find the spot with the least number of unused pieces containing it.
  let spot = findMin $ DS.map (\loc -> (length $ DS.filter (member loc) unused, loc)) uncovered

  guard (fst spot > 0) -- nothing goes here; failed

  -- get each unused piece that covers this spot
  ns <- toList $ DS.filter (member $ snd spot) unused

  let -- remove the spots covered by this piece from the board
      newUncovered = uncovered \\ ns

      -- remove the unused pieces that share a spot with this piece
      newUnused = DS.filter (DS.null . intersection ns) unused

      -- add the piece to the solution being built up.
      newPiecesUsed = ns : used

  return $ Progress newPiecesUsed newUncovered newUnused

nameToPiece :: [[Char]] -> Char -> Piece
nameToPiece image name = 
  let unboundedGrid = [[(row, col) | row <- [0 .. ]] | col <- [0 .. ]]
      indexed = concat $ zipWith zip unboundedGrid image
      pieceCoords = Prelude.filter (\((r, c), n) -> n == name) indexed
  in fromList $ Name name : ((Location . fst) <$> pieceCoords)

initialProgress :: [(Int, Int)] -> [[Char]] -> Progress
initialProgress squares image = 
  let names = DL.nub $ concat image

      pieces = fmap (nameToPiece image) names

      board = fromList $ fmap Name names ++ fmap Location squares
      placements = allPlacements pieces board
  in Progress [] board placements

solve :: Progress -> [ Progress ]
solve progress = do
  if (DS.null $ uncovered progress) -- no space left (solved)
  then return progress
  else solve =<< next progress

solve0 :: [(Int, Int)] -> [[Char]] -> [Progress]
solve0 squares image = 
  solve $ initialProgress squares image

pop2 :: [[a]] -> [[a]]
pop2 xss =
  case xss of
    [_]:xs -> pop2 xs
    (_:ts):xs -> ts : xs
    _ -> xss -- should not happen

step :: State History Progress
step = do
  history <- get
  let newOptions = next $ head $ head history

      newHistory = 
          if newOptions == []
          then pop2 history
          else newOptions : history

  put newHistory
  return $ head $ head $ newHistory

step0 :: [(Int, Int)] -> [[Char]] -> State History Progress
step0 squares image = do
  let progress = initialProgress squares image
  put [[progress]]
  return progress

