{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Miso 
import Miso.String hiding (maximum)
import qualified Miso.Svg as MS (g_, style_, width_, height_, transform_, x_, y_, rect_, svg_, version_)
import Control.Monad.State
import Data.Map

import qualified Solve as S 

data Model = Model
  { time :: Double
  , delta :: Double
  , progress :: S.Progress
  , layout :: [S.Piece]
  , w :: Int
  , h :: Int
  } deriving (Show, Eq)

initialModel :: Model
initialModel =
  Model
  { time = 0
  , delta = 0
  , progress = [] :: S.Progress
  , layout = [] :: S.Layout
  , w = 0
  , h = 0
  }

main :: IO ()
main = do
  t <- now
  startApp App { model = initialModel
               , initialAction = Init t
               , update = updateModel
               , view   = viewModel
               , events = defaultEvents
               , mountPoint = Nothing
               , subs   = []
               }

updateModel :: Action -> Model -> Effect Action Model

updateModel (Init newTime) model@Model {..} = newModel <# (Time <$> now)
  where
    pieces = [ ['I', 'P', 'P', 'Y', 'Y', 'Y', 'Y', 'V', 'V', 'V']
             , ['I', 'P', 'P', 'X', 'Y', 'L', 'L', 'L', 'L', 'V']
             , ['I', 'P', 'X', 'X', 'X', 'F', 'Z', 'Z', 'L', 'V']
             , ['I', 'T', 'W', 'X', 'F', 'F', 'F', 'Z', 'U', 'U']
             , ['I', 'T', 'W', 'W', 'N', 'N', 'F', 'Z', 'Z', 'U']
             , ['T', 'T', 'T', 'W', 'W', 'N', 'N', 'N', 'U', 'U'] ]

    board = [(row, col) | row <- [0..9], col <- [0..5]]

    newW = 1 + (maximum $ fmap snd board)
    newH = 1 + (maximum $ fmap fst board)

    (newLayout,newProgress) = runState (S.step0 board pieces) progress
    newModel = model { time = newTime
                     , progress = newProgress
                     , layout = newLayout
                     , w = newW
                     , h = newH
                     }

updateModel (Time newTime) model@Model {..} = newModel <# (Time <$> now)
  where
    newDelta = newTime - time 
    (newLayout,newProgress) = runState S.step progress
    newModel = model { time = newTime 
                     , delta = newDelta
                     , progress = newProgress
                     ,layout=newLayout
                     }

data Action
  = Init Double
  | Time Double
  deriving (Show, Eq)

viewModel :: Model -> View Action
viewModel x = div_ [] [ viewGame x ]

cellSize :: Int
cellSize = 20

viewGame :: Model -> View Action
viewGame model@Model {..} =
          div_
              []
              [ MS.svg_
                    [ MS.version_ "1.1"
                    , width_ (ms $ show (w * cellSize))
                    , height_ (ms $ show (h * cellSize))
                    ]
                    (Prelude.concatMap showPiece layout)
              ]

showSquare :: (Int,Int) -> View Action
showSquare (row, col) =
    MS.rect_
        [ MS.x_ "0.05"
        , MS.y_ "0.05"
        , MS.width_ "0.9"
        , MS.height_ "0.9"
        , style_ $ fromList [("fill", "red")]
        ]
        []

showCell :: (Int, Int) -> View Action
showCell pos =
    let (row, col) = pos
        scale = show cellSize
    in MS.g_ [ MS.transform_
                (ms $    "scale (" ++ scale ++ ", " ++ scale ++ ") " 
                      ++ "translate (" ++ show col ++ ", " ++ show row ++ ") ")
          ]
          [showSquare pos] 

showPiece :: S.Piece -> [View Action]
showPiece p = 
   let name = S.getName p
       locations = S.getLocations p
   in fmap showCell locations
