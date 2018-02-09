import Prelude (($), print, filter, length, fmap, concat, zipWith, fst, (==), (.), Char, Int, return, (++), maximum, minimum, (+), (-), snd, Bool(False, True), Show, Eq, Ord, not, head, tail, reverse, take, (>))
import Data.Set as DS
import Data.List as DL (sort, nub, repeat)
import Control.Monad
import Control.Monad.Trans.State


data Place = Location (Int,Int) | Name Char deriving (Show, Eq, Ord)

type Piece = Set Place

bounds :: Piece -> ((Int, Int), (Int, Int))
bounds p = let locations = DS.filter (\pl -> case pl of
                                             Location coords -> True
                                             Name _ -> False) p
               coords = fmap (\(Location (row, col)) -> (row,col)) $ toList locations
               rows = fmap fst coords
               cols = fmap snd coords
           in ((minimum rows, minimum cols), (maximum rows, maximum cols))

translateLocation :: (Int, Int) -> Place -> Place
translateLocation (vshift, hshift) loc =
    case loc of
        Location (row, col) -> Location (row+vshift, col+hshift)
        _ -> loc

rotateLocation :: Place -> Place
rotateLocation loc =
    case loc of
        Location (row, col) -> Location (col, -row)
        _ -> loc

flipLocation :: Place -> Place
flipLocation loc =
    case loc of
        Location (row, col) -> Location (col, row)
        _ -> loc

translatePiece :: Piece -> (Int, Int) -> Piece
translatePiece p shift = map (translateLocation shift) p

rotatePiece :: Piece -> Piece
rotatePiece p = map rotateLocation p

flipPiece :: Piece -> Piece
flipPiece p = map flipLocation p

translations :: Set Place -> Piece -> [(Int, Int)]
translations board p = do 
    let ((minRow, minCol),(maxRow, maxCol)) = bounds p
        ((minRowBoard, minColBoard),(maxRowBoard, maxColBoard)) = bounds board
    vt <- [minRowBoard-maxRow..maxRowBoard-minRow]
    ht <- [minColBoard-maxCol..maxColBoard-minCol]
    return (vt, ht)

placements board p = Prelude.filter (`isSubsetOf` board) (fmap (translatePiece p) (translations board p))

fullPlacements board p0 = 
    let p1 = rotatePiece p0
        p2 = rotatePiece p1
        p3 = rotatePiece p2
        r0 = flipPiece p0
        r1 = flipPiece p1
        r2 = flipPiece p2
        r3 = flipPiece p3

        placementsWithDuplicates 
           =  (placements board p0) 
           ++ (placements board p1) 
           ++ (placements board p2) 
           ++ (placements board p3) 
           ++ (placements board r0) 
           ++ (placements board r1) 
           ++ (placements board r2) 
           ++ (placements board r3) 

    in nub placementsWithDuplicates

allPlacements :: [Piece] -> Set Place -> Set Piece
allPlacements pieces board = fromList $ concat $ fmap (fullPlacements board) pieces

nextOpenSpot :: (Set Place, Set Piece) -> [(Piece, (Set Place, Set Piece))]
nextOpenSpot (board, placements) = do
        let spot = findMin $ map  (\loc -> (length $ DS.filter (member loc) placements, loc) ) board
        guard (fst spot > 0)  -- nothing goes here; failed
        ns <- toList $ DS.filter (member $ snd spot) placements
        return (ns, (board \\ ns, DS.filter (null.(intersection ns)) placements))

solve :: (Set Place, Set Piece) -> [ [Piece] ]
solve (board, placements) =
    if null board then return []   -- solved!
    else do
        (ns, (nextBoard, nextPlacements)) <- nextOpenSpot (board, placements)
        subsol <- solve (nextBoard, nextPlacements)
        return (ns:subsol)

solveStateT :: StateT (Set Place, Set Piece) [] [Piece]
solveStateT = do
    (board,_) <- get
    if null board then return []
    else do
        ns <- StateT nextOpenSpot
        subsol <- solveStateT
        return (ns:subsol)

pop2 :: [[a]] -> [[a]]
pop2 xss =
    case xss of
        (_:[]):xs -> pop2 xs 
        (_:ts):xs -> ts:xs
        _ -> xss -- should not happen
    
step :: State ([[(Piece, (Set Place, Set Piece))]]) [Piece]
step = do
    optionStack <- get
    let (_,(board,placements )) = head $ head optionStack
    if null board then 
        put $ pop2 optionStack
    else do
        let ns = nextOpenSpot (board,placements)
        if ns == [] then 
            put $ pop2 optionStack
        else 
            put $ ns:optionStack
    newOptions <- get 
    return $ fmap (fst.head) newOptions

main = do 
    let fullBoard = fromList $ fmap Name names ++ [Location (row, col) | row <- [0..9], col <- [0..5]]

        image = [ ['I', 'P', 'P', 'Y', 'Y', 'Y', 'Y', 'V', 'V', 'V']
                , ['I', 'P', 'P', 'X', 'Y', 'L', 'L', 'L', 'L', 'V']
                , ['I', 'P', 'X', 'X', 'X', 'F', 'Z', 'Z', 'L', 'V']
                , ['I', 'T', 'W', 'X', 'F', 'F', 'F', 'Z', 'U', 'U']
                , ['I', 'T', 'W', 'W', 'N', 'N', 'F', 'Z', 'Z', 'U']
                , ['T', 'T', 'T', 'W', 'W', 'N', 'N', 'N', 'U', 'U'] ]

        indexed = concat $ fmap (\(row, ns) -> zipWith (\col c -> ((row,col),c)) [0..] ns) (zipWith (,) [0..] image)

        names = nub $ concat image

        pieces :: [Piece]
        pieces = fmap (\n -> fromList $ (Name n) : (fmap (Location . fst) $ Prelude.filter (\((r,c),name) -> name==n) indexed)) names


        solutions = runStateT solveStateT (fullBoard, allPlacements pieces fullBoard)

        initialState = [nextOpenSpot (fullBoard, allPlacements pieces fullBoard)]
        steps = sequence $ repeat step
        results = evalState steps initialState
    mapM_ print $ (take 5000) results 

