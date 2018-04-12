module Solve ( allSteps, solve) where

import Control.Monad
import Data.Set as DS 
import Data.Tree

import Types
import Init

next :: Progress -> [Progress]
next (Progress used uncovered unused) = do
  guard (not $  DS.null uncovered) -- no uncovered space left (solved)

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

allSteps :: [(Int, Int)] -> [[Char]] -> [Progress]
allSteps squares image = 
  let ip = initialProgress squares image
  in flatten $ unfoldTree (\p -> (p, next p)) ip

solve :: [(Int, Int)] -> [[Char]] -> [Progress]
solve squares image = 
  Prelude.filter ((DS.null) . uncovered ) $ allSteps squares image

