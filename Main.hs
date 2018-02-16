{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Miso 
import qualified Miso.String as MST (ms)
import qualified Miso.Svg as MSV (g_, style_, width_, height_, transform_, x_, y_, rect_, svg_, version_)
import Control.Monad.State
import Data.Map as DM

import qualified Solve as S 

data Action
  = Init Double
  | Time Double
  | SetRate Rate
  | RequestStep
  deriving (Show, Eq)

data Rate = Fast | Slow | Step deriving (Show, Eq)

data Model = Model
  { time :: Double
  , progress :: S.Progress
  , layout :: S.Layout
  , solutions :: [S.Layout]
  , w :: Int
  , h :: Int
  , rate :: Rate
  , stepRequested :: Bool
  } deriving (Show, Eq)

main :: IO ()
main = do
  let initialModel = Model 
                       { time = 0
                       , progress = [] 
                       , layout = S.Incomplete [] 
                       , solutions = []
                       , w = 0
                       , h = 0
                       , rate = Step
                       , stepRequested = False
                       }
  t <- Miso.now
  Miso.startApp Miso.App 
    { model = initialModel
    , initialAction = Init t
    , update = updateModel
    , view   = viewModel 
    , events = Miso.defaultEvents
    , mountPoint = Nothing
    , subs   = []
    }

updateModel :: Action -> Model -> Miso.Effect Action Model

updateModel (SetRate newRate) model = noEff (model {rate=newRate})

updateModel RequestStep model = noEff (model {stepRequested=True})

updateModel (Init newTime) model@Model {..} = newModel <# (Time <$> Miso.now)
  where
    pieces = [ ['I', 'P', 'P', 'Y', 'Y', 'Y', 'Y', 'V', 'V', 'V']
             , ['I', 'P', 'P', 'X', 'Y', 'L', 'L', 'L', 'L', 'V']
             , ['I', 'P', 'X', 'X', 'X', 'F', 'Z', 'Z', 'L', 'V']
             , ['I', 'T', 'W', 'X', 'F', 'F', 'F', 'Z', 'U', 'U']
             , ['I', 'T', 'W', 'W', 'N', 'N', 'F', 'Z', 'Z', 'U']
             , ['T', 'T', 'T', 'W', 'W', 'N', 'N', 'N', 'U', 'U'] ]

    board = [(row, col) | row <- [0..11], col <- [0..4]]

    newW = 1 + (maximum $ fmap snd board)
    newH = 1 + (maximum $ fmap fst board)

    (newLayout,newProgress) = runState (S.step0 board pieces) progress

    newSolutions = case newLayout of
                       S.Complete _ -> newLayout:solutions
                       S.Incomplete _ -> solutions

    newModel = model { time = newTime
                     , progress = newProgress
                     , layout = newLayout
                     , solutions = newSolutions
                     , w = newW
                     , h = newH
                     }

updateModel (Time newTime) model@Model {..} = newModel <# (Time <$> Miso.now)
  where
    newDelta = newTime - time 
    (newLayout,newProgress) = runState S.step progress

    newSolutions = case newLayout of
                       S.Complete _ -> newLayout:solutions
                       S.Incomplete _ -> solutions

    newModel = model { time = newTime 
                     , progress = newProgress
                     , solutions = newSolutions
                     , layout = newLayout
                     }

viewModel :: Model -> Miso.View Action
viewModel model@Model {..} =
  Miso.div_
      []
      ( viewControls rate
      : viewLayout workCellSize w h layout 
      : fmap (viewLayout solutionCellSize w h) 
             (Prelude.reverse solutions) 
      )
  where workCellSize = 30
        solutionCellSize = ((workCellSize * 2)`div` 3)

viewControls :: Rate -> Miso.View Action
viewControls rate = 
  div_ 
    []
    ([ input_ [type_ "radio", name_ "updateRate", checked_ (rate==Fast), onClick (SetRate Fast)] [], text "Fast"
    , input_ [type_ "radio", name_ "updateRate", checked_ (rate==Slow), onClick (SetRate Slow)] [], text "Slow"
    , input_ [type_ "radio", name_ "updateRate", checked_ (rate==Step), onClick (SetRate Step)] [], text "Step"
    ] ++ if (rate==Step) then [button_ [onClick RequestStep] [text "Step"]] else[])


viewLayout :: Int -> Int -> Int -> S.Layout -> Miso.View Action
viewLayout cellSize width height layout =
  div_ [class_ "svg-container"]
       [ MSV.svg_
           [ MSV.version_ "1.1"
           , MSV.width_ (MST.ms $ show (width * cellSize))
           , MSV.height_ (MST.ms $ show (height * cellSize))
           ]
           (Prelude.concatMap (showPiece cellSize) pieces)
       ]
  where pieces = case layout of
                     S.Complete p -> p
                     S.Incomplete p -> p

showPiece :: Int -> S.Piece -> [Miso.View Action]
showPiece cellSize p = 
   let name = S.getName p
       locations = S.getLocations p
   in fmap (showCell cellSize (getColor name)) locations

showCell :: Int -> String -> (Int, Int) -> Miso.View Action
showCell cellSize color (row,col) =
    MSV.g_ [ MSV.transform_
                (MST.ms $    "scale (" ++ scale ++ ", " ++ scale ++ ") " 
                          ++ "translate (" ++ show col ++ ", " ++ show row ++ ") ")
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
    where scale = show cellSize

getColor :: Char -> String
getColor name = 
  let colorMap = fromList [ ('F', "green")
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
  in case DM.lookup name colorMap of
       Just color -> color
       Nothing -> "black"  -- should not happen.

