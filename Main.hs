import Prelude (($), print, filter, length, fmap, concat, zipWith, fst, (==), (.), Char, Integer, return, (++), maximum, minimum, (+), (-), snd, Bool(False, True), Show, Eq, Ord, not, head, tail, reverse, take, (>))
import Data.Set as DS
import Data.List (sort, nub)
import Control.Monad


data Place = Location (Integer,Integer) | Name Char deriving (Show, Eq, Ord)

type Piece = Set Place

bounds :: Piece -> ((Integer, Integer), (Integer, Integer))
bounds p = let locations = DS.filter (\pl -> case pl of
                                             Location coords -> True
                                             Name _ -> False) p
               coords = fmap (\(Location (row, col)) -> (row,col)) $ toList locations
               rows = fmap fst coords
               cols = fmap snd coords
           in ((minimum rows, minimum cols), (maximum rows, maximum cols))

translateLocation :: (Integer, Integer) -> Place -> Place
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

translatePiece :: Piece -> (Integer, Integer) -> Piece
translatePiece p shift = map (translateLocation shift) p

rotatePiece :: Piece -> Piece
rotatePiece p = map rotateLocation p

flipPiece :: Piece -> Piece
flipPiece p = map flipLocation p

translations :: Set Place -> Piece -> [(Integer, Integer)]
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
           =  (placements board p0) ++ (placements board p1) ++ (placements board p2) ++ (placements board p3) 
           ++ (placements board r0) ++ (placements board r1) ++ (placements board r2) ++ (placements board r3) 
    in nub placementsWithDuplicates

board :: Set Place
board = fromList $ fmap Name names ++ do
    row <- [0..9]
    col <- [0..5]
    return $ Location (row, col)

image = [ ['I', 'P', 'P', 'Y', 'Y', 'Y', 'Y', 'V', 'V', 'V']
        , ['I', 'P', 'P', 'X', 'Y', 'L', 'L', 'L', 'L', 'V']
        , ['I', 'P', 'X', 'X', 'X', 'F', 'Z', 'Z', 'L', 'V']
        , ['I', 'T', 'W', 'X', 'F', 'F', 'F', 'Z', 'U', 'U']
        , ['I', 'T', 'W', 'W', 'N', 'N', 'F', 'Z', 'Z', 'U']
        , ['T', 'T', 'T', 'W', 'W', 'N', 'N', 'N', 'U', 'U'] ]

indexed :: [((Integer, Integer), Char)]
indexed = concat $ fmap (\(row, ns) -> zipWith (\col c -> ((row,col),c)) [0..] ns) (zipWith (,) [0..] image)

names = nub $ concat image

pieces :: [Piece]
pieces = fmap (\n -> fromList $ (Name n) : (fmap (Location . fst) $ Prelude.filter (\((r,c),name) -> name==n) indexed)) names

allPlacements :: Set Piece
allPlacements = fromList $ concat $ fmap (fullPlacements board) pieces

solve :: Set Place -> Set Piece -> [ [Piece] ]
solve board placements = 
    if null board then return []   -- solved!
    else do 
          let spot = findMin $ map  (\loc -> (length $ DS.filter (member loc) placements, loc) ) board
          guard (fst spot > 0)  -- nothing goes here; failed
          let onSpot = DS.filter (member $ snd spot) placements
          p <- toList onSpot
          subsol <- solve (board \\ p) (DS.filter (null.(intersection p)) placements) 
          return (p:subsol)

main = do 
           let solutions = solve board allPlacements
           print $ head solutions
           print $ head $ tail solutions
           forM_ (take 10 solutions) $ print 

