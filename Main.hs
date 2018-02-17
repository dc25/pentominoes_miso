{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.State
import Data.Map as DM
import qualified Data.Set as DS (null)
import Data.Maybe
import Miso
import qualified Miso.String as MST (ms)
import qualified Miso.Svg as MSV ( g_ , height_ , rect_ , style_ , svg_ , transform_ , version_ , width_ , x_ , y_)

import qualified Solve as S

data Action
  = Init Double
  | Time Double
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
  , progress :: S.Progress
  , solution :: S.Solution
  , completeSolutions :: [S.Solution]
  , w :: Int
  , h :: Int
  , rate :: Rate
  , stepRequested :: Bool
  } deriving Eq

main :: IO ()
main = do
  let initialModel =
        Model
          { time = 0
          , progress = []
          , solution = S.Solution [] mempty
          , completeSolutions = []
          , w = 0
          , h = 0
          , rate = Step
          , stepRequested = False
          }
  t <- Miso.now
  Miso.startApp
    Miso.App
      { model = initialModel
      , initialAction = Init t
      , update = updateModel
      , view = viewModel
      , events = Miso.defaultEvents
      , mountPoint = Nothing
      , subs = []
      }

updateModel :: Action -> Model -> Miso.Effect Action Model

updateModel (SetRate newRate) model = noEff (model {rate = newRate})

updateModel RequestStep model = noEff (model {stepRequested = True})

updateModel (Init newTime) model@Model {..} = newModel <# (Time <$> Miso.now)
  where
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

    (newSolution, newProgress) = runState (S.step0 board pieces) progress

    newModel =
      model
        { time = newTime
        , progress = newProgress
        , solution = newSolution
        , w = newW
        , h = newH
        }

updateModel (Time newTime) model@Model {..} = newModel <# (Time <$> Miso.now)
  where
    newDelta = newTime - time

    (newSolution@(S.Solution newPieces newRemainder), newProgress) = runState S.step progress

    newCompleteSolutions =
      if (DS.null newRemainder)
      then newSolution : completeSolutions
      else completeSolutions

    newModel =
      model
        { time = newTime
        , progress = newProgress
        , completeSolutions = newCompleteSolutions
        , solution = newSolution
        }

viewModel :: Model -> Miso.View Action
viewModel model@Model {..} =
  Miso.div_ []
    ( viewControls rate 
    : viewSolution workCellSize w h solution 
    : fmap (viewSolution solutionCellSize w h) (Prelude.reverse completeSolutions))
  where
    workCellSize = 30
    solutionCellSize = (workCellSize * 2) `div` 3

viewControls :: Rate -> Miso.View Action
viewControls rate =
  div_ []
    ([ input_ [ type_ "radio" , name_ "updateRate" , checked_ (rate == Fast) , onClick (SetRate Fast) ] [] , text "Fast"
     , input_ [ type_ "radio" , name_ "updateRate" , checked_ (rate == Slow) , onClick (SetRate Slow) ] [] , text "Slow"
     , input_ [ type_ "radio" , name_ "updateRate" , checked_ (rate == Step) , onClick (SetRate Step) ] [] , text "Step"
     ] ++ [button_ [onClick RequestStep] [text "Step"] | rate == Step]
    )

viewSolution :: Int -> Int -> Int -> S.Solution -> Miso.View Action
viewSolution cellSize width height (S.Solution pieces _) =
  div_
    []
    [ MSV.svg_
        [ MSV.version_ "1.1"
        , MSV.width_ (MST.ms $ show (width * cellSize))
        , MSV.height_ (MST.ms $ show (height * cellSize))
        ]
        (Prelude.concatMap (showPiece cellSize) pieces)
    ]

showPiece :: Int -> S.Piece -> [Miso.View Action]
showPiece cellSize p =
  let name = S.getName p
      locations = S.getLocations p
   in fmap (showCell cellSize (getColor name)) locations

showCell :: Int -> String -> (Int, Int) -> Miso.View Action
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
