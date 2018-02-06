import Data.Map as DM 
import Data.Set as DS 
import Data.List (sort, nub)


data Place = Location (Integer,Integer) | Name Char deriving (Show, Eq, Ord)

type Piece = DS.Set Place

bounds :: Piece -> ((Integer, Integer), (Integer, Integer))
bounds p = let locations = DS.filter (\pl -> case pl of
                                             Location coords -> True
                                             Name _ -> False) p
               coords = fmap (\(Location (row, col)) -> (row,col)) $ DS.toList locations
               rows = fmap fst coords
               cols = fmap snd coords
           in ((minimum rows, minimum cols), (maximum rows, maximum cols))

translateLocation (vshift, hshift) loc =
    case loc of
        Location (row, col) -> Location (row+vshift, col+hshift)
        _ -> loc

rotateLocation loc =
    case loc of
        Location (row, col) -> Location (col, -row)
        _ -> loc

flipLocation loc =
    case loc of
        Location (row, col) -> Location (col, row)
        _ -> loc

translatePiece p shift = DS.map (translateLocation shift) p

rotatePiece p = DS.map rotateLocation p

flipPiece p = DS.map flipLocation p

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
board = DS.fromList $ fmap Name names ++ do
    row <- [0..9]
    col <- [0..5]
    return $ Location (row, col)

image = [ ['I', 'P', 'P', 'Y', 'Y', 'Y', 'Y', 'V', 'V', 'V']
        , ['I', 'P', 'P', 'X', 'Y', 'L', 'L', 'L', 'L', 'V']
        , ['I', 'P', 'X', 'X', 'X', 'F', 'Z', 'Z', 'L', 'V']
        , ['I', 'T', 'W', 'X', 'F', 'F', 'F', 'Z', 'U', 'U']
        , ['I', 'T', 'W', 'W', 'N', 'N', 'F', 'Z', 'Z', 'U']
        , ['T', 'T', 'T', 'W', 'W', 'N', 'N', 'N', 'U', 'U'] ]

indexed :: Map (Integer, Integer) Char
indexed = DM.fromList $ concat $ fmap (\(row, ns) -> zipWith (\col c -> ((row,col),c)) [0..] ns) (zipWith (,) [0..] image)

names = nub $ fmap snd $ DM.toList indexed

pieces :: [Piece]
pieces = fmap (\n -> DS.fromList $ (Name n) : (fmap (Location . fst) $ DM.toList $ DM.filter (==n) indexed)) names

allPlacements = concat $ fmap (fullPlacements board) pieces

solve board placements =
    -- DS.findMin $ DS.map  (\loc -> (length $ Prelude.filter (DS.member loc) placements, loc) ) board
    sort $ DS.toList $ DS.map  (\loc -> (length $ Prelude.filter (DS.member loc) placements, loc) ) board

main = print $ solve board allPlacements

