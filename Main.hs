import Data.Set 
import Data.List 
import Control.Monad
import Control.Monad.Trans.State

import Solve

main = do 
    let image = [ ['I', 'P', 'P', 'Y', 'Y', 'Y', 'Y', 'V', 'V', 'V']
                , ['I', 'P', 'P', 'X', 'Y', 'L', 'L', 'L', 'L', 'V']
                , ['I', 'P', 'X', 'X', 'X', 'F', 'Z', 'Z', 'L', 'V']
                , ['I', 'T', 'W', 'X', 'F', 'F', 'F', 'Z', 'U', 'U']
                , ['I', 'T', 'W', 'W', 'N', 'N', 'F', 'Z', 'Z', 'U']
                , ['T', 'T', 'T', 'W', 'W', 'N', 'N', 'N', 'U', 'U'] ]

        indexed = concat $ fmap (\(row, ns) -> zipWith (\col c -> ((row,col),c)) [0..] ns) (zipWith (,) [0..] image)

        names = nub $ concat image

        pieces = fmap (\n -> fromList $ (Name n) : (fmap (Location . fst) $ Prelude.filter (\((r,c),name) -> name==n) indexed)) names

        fullBoard = fromList $ fmap Name names ++ [Location (row, col) | row <- [0..9], col <- [0..5]]

        solutions = runStateT solveStateT (fullBoard, allPlacements pieces fullBoard)

        initialState = [nextMoves (fullBoard, allPlacements pieces fullBoard)]
        steps = sequence $ repeat step
        results = evalState steps initialState
    mapM_ print $ (take 5000) results 

