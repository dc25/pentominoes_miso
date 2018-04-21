module Solve ( steps, solutions, Progress(..) ) where

import Control.Monad
import Data.Set as DS 
import Data.Tree

data Progress a = Progress { used :: [Set a]  -- solution in progress
                           , uncovered :: Set a -- still to cover
                           , unused :: Set (Set a) -- pieces available
                           } 

next :: Ord a => Progress a -> [Progress a]
next (Progress used uncovered unused) = do
  guard (not $  DS.null uncovered) -- no uncovered space left; solved

  -- find the element with the least number of unused pieces containing it.
  let coverageCounts = DS.map (\el -> (length $ DS.filter (member el) unused, el)) uncovered
      (minCount, minCountCell) = findMin coverageCounts

  guard (minCount > 0) -- nothing goes here; failed

  -- get each unused piece that covers this element
  ns <- toList $ DS.filter (member minCountCell) unused

  let -- remove the cells covered by this piece from the board
      newUncovered = uncovered \\ ns

      -- remove the unused pieces that share a cell with this piece
      newUnused = DS.filter (DS.null . intersection ns) unused

      -- add the piece to the solution being built up.
      newUsed = ns : used

  return $ Progress newUsed newUncovered newUnused

steps :: Ord a => Progress a -> [Progress a]
steps initial = 
  flatten $ unfoldTree (\p -> (p, next p)) initial

solutions :: Ord a => Progress a -> [Progress a]
solutions initial = 
  Prelude.filter (DS.null . uncovered ) $ steps initial

