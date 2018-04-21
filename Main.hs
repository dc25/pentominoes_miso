{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Map as DM (lookup, fromList)
import qualified Data.Set as DS (null, unions)
import Data.Maybe

import Miso
import Miso.String (ms, MisoString, append)
import qualified Miso.Svg as MSV ( g_ , height_ , rect_ , svg_ , transform_ , version_ , width_ , x_ , y_, style_)

import Piece
import Init
import Solve (steps, Progress(..))

data Rate
  = Fast
  | Slow
  | Step
  deriving (Eq)

data Action
  = Time Double
  | SetRate Rate
  | RequestStep
  deriving (Eq)

data Model = Model
  { time :: Double
  , steps :: [Progress Cell]
  , solutions :: [Progress Cell]
  , rate :: Rate
  , stepRequested :: Bool
  } 

-- Model comparision using derived Eq runs the risk 
-- of comparing long lists of steps which causes app 
-- to freeze.  Model comparison is only used to 
-- prevent unnecessary view refreshes so, worst case, 
-- this may result in more DOM diffing.
instance Eq Model where
  _ == _ = False

main :: IO ()
main = do
  t <- now
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

      allSteps = Solve.steps zeroProgress

      initialModel =
        Model
          { time = 0
          , steps = allSteps
          , solutions = []
          , rate = Step
          , stepRequested = False
          }

  startApp
    App
      { model = initialModel
      , initialAction = Time t
      , update = updateModel
      , view = viewModel
      , events = defaultEvents
      , mountPoint = Nothing
      , subs = []
      }

updateModel :: Action -> Model -> Effect Action Model

updateModel (SetRate newRate) model = Effect (model {rate = newRate}) []

updateModel RequestStep model = Effect (model {stepRequested = True}) []

updateModel (Time nTime) model@Model {..} = Effect newModel [Time <$> now]
  where
    delta = nTime - time
    (newSteps, newSolutions, newTime) = 
        if      ((rate == Slow) && (delta < 400.0))
           ||   ((rate == Step) && not stepRequested)
        then (steps, solutions, time)
        else (nSteps, nSolutions, nTime)
             where 
               nSteps = tail steps
               currentStep = head nSteps
               -- if no cells remain uncovered, we have a solution
               -- so add it to the list of solutions.
               nSolutions =
                 if DS.null $ uncovered currentStep
                 then currentStep : solutions
                 else solutions
        
    newModel =
      model
        { time = newTime 
        , Main.steps = newSteps
        , solutions = newSolutions
        , stepRequested = False
        }

viewModel :: Model -> View Action
viewModel Model {..} =
  div_ []
    ( viewControls rate 
    : viewProgress  workCellSize (head steps)
    : fmap (viewProgress solutionCellSize) (reverse solutions))
  where
    workCellSize = 30
    solutionCellSize = (workCellSize * 2) `div` 3

viewControls :: Rate -> View Action
viewControls rate =
  div_ []
    ([ input_ [ type_ "radio" , name_ "updateRate" , checked_ (rate == Fast) , onClick (SetRate Fast) ] [] , text "Fast"
     , input_ [ type_ "radio" , name_ "updateRate" , checked_ (rate == Slow) , onClick (SetRate Slow) ] [] , text "Slow"
     , input_ [ type_ "radio" , name_ "updateRate" , checked_ (rate == Step) , onClick (SetRate Step) ] [] , text "Step"
     ] ++ [button_ [onClick RequestStep] [text "Step"] | rate == Step]
    )

viewProgress :: Int -> Progress Cell -> View Action
viewProgress cellSize (Progress used uncovered _) =
  div_
    []
    [ MSV.svg_
        [ MSV.version_ "1.1"
        , MSV.width_ (ms (w * cellSize))
        , MSV.height_ (ms (h * cellSize))
        ]
        (concatMap (showPiece cellSize) used)
    ]
  where
    cells = DS.unions (uncovered : used)
    ((rMin, cMin), (rMax, cMax)) = bounds $ getLocations cells
    w = 1 + cMax - cMin 
    h = 1 + rMax - rMin 

showPiece :: Int -> Piece -> [View Action]
showPiece cellSize p =
  fmap (showCell cellSize (getColor p)) (getLocations p)

showCell :: Int -> MisoString -> (Int, Int) -> View Action
showCell cellSize color (row, col) =
  MSV.g_
    [ MSV.transform_ $ scaleTransform `append` translateTransform
    ]
    [ MSV.rect_
        [ MSV.x_ "0.05"
        , MSV.y_ "0.05"
        , MSV.width_ "0.9"
        , MSV.height_ "0.9"

          -- Couldn't get MSV.style_ to work so using Miso.style_ 
        , Miso.style_ $ fromList [("fill", color)]
        ]
        []
    ]
  where
    scale = ms cellSize
    scaleTransform = "scale (" `append` scale `append` ", " `append` scale `append` ") " 
    translateTransform = "translate (" `append` ms col `append` ", " `append` ms row `append` ") "

getColor :: Piece -> MisoString
getColor piece =
  let name = getName piece
      colorMap =
        fromList
          [ ('F', "green")
          , ('I', "blue")
          , ('L', "red")
          , ('N', "yellow")
          , ('P', "purple")
          , ('T', "brown")
          , ('U', "maroon")
          , ('V', "cyan")
          , ('W', "pink")
          , ('X', "orange")
          , ('Y', "navy")
          , ('Z', "lime")
          ]
   in fromMaybe "black" (DM.lookup name colorMap)
