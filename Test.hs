
import Data.Set
import Data.List.Split
import Data.List

import Piece
import Init
import Solve

main = do
  let pieces =
        [ ['I', 'P', 'P', 'Y', 'Y', 'Y', 'Y', 'V', 'V', 'V']
        , ['I', 'P', 'P', 'X', 'Y', 'L', 'L', 'L', 'L', 'V']
        , ['I', 'P', 'X', 'X', 'X', 'F', 'Z', 'Z', 'L', 'V']
        , ['I', 'T', 'W', 'X', 'F', 'F', 'F', 'Z', 'U', 'U']
        , ['I', 'T', 'W', 'W', 'N', 'N', 'F', 'Z', 'Z', 'U']
        , ['T', 'T', 'T', 'W', 'W', 'N', 'N', 'N', 'U', 'U']
        ]
      board = [(row, col) | row <- [0 .. 11], col <- [0 .. 4]]

      zeroProgress :: Progress Cell
      zeroProgress = initialProgress board pieces 

  print $ "Piece count = " ++ show (length $ unused zeroProgress)

  let firstSolution :: [Piece]
      firstSolution =   used -- the pieces used for the first solution
                      $ head -- the first solution
                      $ solutions -- get the list of solutions
                      $ zeroProgress -- no progress yet

  mapM_ print firstSolution

  let decorate piece = [ (loc,name) | loc <- getLocations piece, let name = getName piece ]

      prettySolution :: [[Char]]
      prettySolution =  chunksOf 5 -- 5 columns at a time for printing.
                      $ fmap snd   -- Drop the locations - keep the names.
                      $ sort       -- Sort by location.
                      $ concat     -- gather all the named locations 
                      $ decorate <$> firstSolution -- Add names to locations
  return ()

  -- mapM_ print prettySolution

