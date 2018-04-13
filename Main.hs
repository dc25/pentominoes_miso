{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Map as DM (lookup, fromList)
import qualified Data.Set as DS (null)
import Data.Maybe

import Miso
import qualified Miso.String as MST (ms)
import qualified Miso.Svg as MSV ( g_ , height_ , rect_ , svg_ , transform_ , version_ , width_ , x_ , y_, style_)

import Types 
import Utilities
import Solve 

data Action
  = Time Double
  | SetRate Rate
  | RequestStep
  deriving (Show, Eq)

data Rate
  = Fast
  | Slow
  | Step
  deriving (Show, Eq)

data Model = Model
  { time :: Double
  , steps :: [Progress]
  , solutions :: [Progress]
  , w :: Int
  , h :: Int
  , rate :: Rate
  , stepRequested :: Bool
  } 

instance Eq Model where
  _ == _ = False

main :: IO ()
main = do
  t <- now
  let
    pieces =
      [ ['I', 'P', 'P', 'Y', 'Y', 'Y', 'Y', 'V', 'V', 'V']
      , ['I', 'P', 'P', 'X', 'Y', 'L', 'L', 'L', 'L', 'V']
      , ['I', 'P', 'X', 'X', 'X', 'F', 'Z', 'Z', 'L', 'V']
      , ['I', 'T', 'W', 'X', 'F', 'F', 'F', 'Z', 'U', 'U']
      , ['I', 'T', 'W', 'W', 'N', 'N', 'F', 'Z', 'Z', 'U']
      , ['T', 'T', 'T', 'W', 'W', 'N', 'N', 'N', 'U', 'U']
      ]
    board = [(row, col) | row <- [0 .. 11], col <- [0 .. 4]]

    newW = 1 + maximum (fmap snd board)
    newH = 1 + maximum (fmap fst board)

  let initialModel =
        Model
          { time = 0
          , steps = allSteps board pieces 
          , solutions = []
          , w = newW
          , h = newH
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

updateModel (SetRate newRate) model = noEff (model {rate = newRate})

updateModel RequestStep model = noEff (model {stepRequested = True})

updateModel (Time nTime) model@Model {..} = newModel <# (Time <$> now)
  where
    delta = nTime - time
    (newSteps, newSolutions, newTime) = 
        if      ((rate == Slow) && (delta < 400.0))
           ||   ((rate == Step) && (not stepRequested))
        then (steps, solutions, time)
        else (nSteps, nSolutions, nTime)
             where 
               nSteps = tail steps
               currentStep = head nSteps
               -- if no spots remain uncovered, we have a solution
               -- so add it to the list of solutions.
               nSolutions =
                 if (DS.null $ uncovered currentStep)
                 then currentStep : solutions
                 else solutions
        
    newModel =
      model
        { time = newTime 
        , steps = newSteps
        , solutions = newSolutions
        , stepRequested = False
        }

viewModel :: Model -> View Action
viewModel Model {..} =
  div_ []
    ( viewControls rate 
    : viewProgress  workCellSize w h (head steps)
    : fmap (viewProgress solutionCellSize w h) (reverse solutions))
  where
    workCellSize = 36
    solutionCellSize = (workCellSize * 2) `div` 3

viewControls :: Rate -> View Action
viewControls rate =
  div_ []
    ([ input_ [ type_ "radio" , name_ "updateRate" , checked_ (rate == Fast) , onClick (SetRate Fast) ] [] , text "Fast"
     , input_ [ type_ "radio" , name_ "updateRate" , checked_ (rate == Slow) , onClick (SetRate Slow) ] [] , text "Slow"
     , input_ [ type_ "radio" , name_ "updateRate" , checked_ (rate == Step) , onClick (SetRate Step) ] [] , text "Step"
     ] ++ [button_ [onClick RequestStep] [text "Step"] | rate == Step]
    )

viewProgress :: Int -> Int -> Int -> Progress -> View Action
viewProgress cellSize width height (Progress pieces _ _) =
  div_
    []
    [ MSV.svg_
        [ MSV.version_ "1.1"
        , MSV.width_ (MST.ms $ show (width * cellSize))
        , MSV.height_ (MST.ms $ show (height * cellSize))
        ]
        (concatMap (showPiece cellSize) pieces)
    ]

showPiece :: Int -> Piece -> [View Action]
showPiece cellSize p =
  let name = getName p
      locations = getLocations p
  in fmap (showCell cellSize (getColor name)) locations

showCell :: Int -> String -> (Int, Int) -> View Action
showCell cellSize color (row, col) =
  MSV.g_
    [ MSV.transform_
        (MST.ms $ "scale (" ++ scale ++ ", " ++ scale ++ ") " ++ "translate (" ++ show col ++ ", " ++ show row ++ ") ")
    ]
    [ MSV.rect_
        [ MSV.x_ "0.05"
        , MSV.y_ "0.05"
        , MSV.width_ "0.9"
        , MSV.height_ "0.9"

          -- Couldn't get MSV.style_ to work so using Miso.style_ instead
          -- which seems to do the trick.
        , Miso.style_ $ fromList [("fill", MST.ms color)]

        ]
        []
    ]
  where
    scale = show cellSize

getColor :: Char -> String
getColor name =
  let colorMap =
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
