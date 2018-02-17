{-# LANGUAGE ScopedTypeVariables #-}

module Solve ( Progress , Piece , Solution(..) , step0 , step , getName , getLocations) where

import Control.Monad
import Control.Monad.State
import Data.List as DL (nub, repeat, sort)
import Data.Set as DS

data Place
  = Location (Int, Int)
  | Name Char
  deriving (Show, Eq, Ord)

type Piece = Set Place

type Board = Set Place

type Puzzle = (Board, Set Piece)

data Solution = Solution { pieces :: [Piece]
                         , remainder :: Board
                         } deriving Eq

type Progress = [[(Solution, Puzzle)]]

isLocation :: Place -> Bool
isLocation pl =
  case pl of
    Location coords -> True
    Name _ -> False

isName :: Place -> Bool
isName = not . isLocation

bounds :: Piece -> ((Int, Int), (Int, Int))
bounds p =
  let locations = DS.filter isLocation p
      coords = (\(Location (row, col)) -> (row, col)) <$> toList locations
      rows = fmap fst coords
      cols = fmap snd coords
   in ((minimum rows, minimum cols), (maximum rows, maximum cols))

translateLocation :: (Int, Int) -> Place -> Place
translateLocation (vshift, hshift) loc =
  case loc of
    Location (row, col) -> Location (row + vshift, col + hshift)
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
translatePiece p shift = DS.map (translateLocation shift) p

rotatePiece :: Piece -> Piece
rotatePiece p = DS.map rotateLocation p

flipPiece :: Piece -> Piece
flipPiece p = DS.map flipLocation p

translations :: Set Place -> Piece -> [(Int, Int)]
translations board p = do
  let ((minRow, minCol), (maxRow, maxCol)) = bounds p
      ((minRowBoard, minColBoard), (maxRowBoard, maxColBoard)) = bounds board
  vt <- [minRowBoard - maxRow .. maxRowBoard - minRow]
  ht <- [minColBoard - maxCol .. maxColBoard - minCol]
  return (vt, ht)

placements board p =
  Prelude.filter
    (`isSubsetOf` board)
    (fmap (translatePiece p) (translations board p))

fullPlacements board p0 =
  let p1 = rotatePiece p0
      p2 = rotatePiece p1
      p3 = rotatePiece p2
      r0 = flipPiece p0
      r1 = flipPiece p1
      r2 = flipPiece p2
      r3 = flipPiece p3

      placementsWithDuplicates =
           placements board p0 
        ++ placements board p1 
        ++ placements board p2 
        ++ placements board p3 
        ++ placements board r0 
        ++ placements board r1 
        ++ placements board r2 
        ++ placements board r3

   in nub placementsWithDuplicates

allPlacements :: [Piece] -> Set Place -> Set Piece
allPlacements pieces board = fromList $ concatMap (fullPlacements board) pieces

nextMoves :: Solution -> Puzzle -> [(Solution, Puzzle)]
nextMoves (Solution pieces remainder) (board, placements) = do
  guard (not $  DS.null board) -- no space left (solved)

  -- find the spot with the least number of pieces containing it.
  let spot = findMin $ DS.map (\loc -> (length $ DS.filter (member loc) placements, loc)) board

  guard (fst spot > 0) -- nothing goes here; failed

  -- get each piece that covers this spot
  ns <- toList $ DS.filter (member $ snd spot) placements

  let -- remove the spots covered by this piece from the board
      newBoard = board \\ ns

      -- remove the placements that share a spot with this piece
      newPlacements = DS.filter (DS.null . intersection ns) placements

      -- add the piece to the solution being built up.
      newPiecesUsed = ns : pieces

      newSolution = Solution newPiecesUsed newBoard

  return (newSolution, (newBoard, newPlacements))

step0 :: [(Int, Int)] -> [[Char]] -> State Progress Solution
step0 squares image = do
  let unboundedGrid = [[(row, col) | row <- [0 .. ]] | col <- [0 .. ]]
  
      indexed = concat $ zipWith zip unboundedGrid image

      names = nub $ concat image

      pieces = fmap
          (\n ->
             fromList $
             Name n : ((Location . fst) <$> Prelude.filter (\((r, c), name) -> name == n) indexed))
          names

      board = fromList $ fmap Name names ++ fmap Location squares
      placements = allPlacements pieces board
      layout = Solution [] board
  put [[(layout, (board, placements))]]
  return layout

step :: State Progress Solution
step = do
  optionStack <- get
  let (piecesUsed, (board, placements)) = head $ head optionStack
      ns = nextMoves piecesUsed (board, placements)
  if Prelude.null ns 
  then put $ pop2 optionStack
  else put $ ns : optionStack
  newOptions <- get
  return $ (fst . head . head) newOptions

pop2 :: [[a]] -> [[a]]
pop2 xss =
  case xss of
    [_]:xs -> pop2 xs
    (_:ts):xs -> ts : xs
    _ -> xss -- should not happen

getName :: Piece -> Char
getName p =
  let Name ch = head $ toList $ DS.filter isName p
   in ch

getLocation :: Place -> (Int, Int)
getLocation pl =
  let Location lo = pl
   in lo

getLocations :: Piece -> [(Int, Int)]
getLocations = fmap getLocation . toList . DS.filter isLocation
