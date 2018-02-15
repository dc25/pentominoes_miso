{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Miso 
import Miso.String hiding (maximum)
import qualified Miso.Svg as MS (g_, style_, width_, height_, transform_, x_, y_, rect_, svg_, version_)
import Control.Monad.State
import Data.Map as DM

import qualified Solve as S 

data Action
  = Init Double
  | Time Double
  deriving (Show, Eq)

data Model = Model
  { time :: Double
  , progress :: S.Progress
  , layout :: S.Layout
  , w :: Int
  , h :: Int
  } deriving (Show, Eq)

initialModel :: Model
initialModel =
  Model
  { time = 0
  , progress = [] :: S.Progress
  , layout = S.Incomplete [] :: S.Layout
  , w = 0
  , h = 0
  }

main :: IO ()
main = do
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
    newModel = model { time = newTime
                     , progress = newProgress
                     , layout = newLayout
                     , w = newW
                     , h = newH
                     }

updateModel (Time newTime) model@Model {..} = newModel <# (Time <$> Miso.now)
  where
    newDelta = newTime - time 
    (newLayout,newProgress) = runState S.step progress
    newModel = model { time = newTime 
                     , progress = newProgress
                     , layout = newLayout
                     }

cellSize :: Int
cellSize = 20

viewModel :: Model -> Miso.View Action
viewModel model@Model {..} =
  Miso.div_
      []
      [ MS.svg_
          [ MS.version_ "1.1"
          , MS.width_ (ms $ show (w * cellSize))
          , MS.height_ (ms $ show (h * cellSize))
          ]
          (Prelude.concatMap showPiece pieces)
      ]
  where pieces = case layout of
                     S.Complete p -> p
                     S.Incomplete p -> p

showPiece :: S.Piece -> [Miso.View Action]
showPiece p = 
   let name = S.getName p
       locations = S.getLocations p
   in fmap (showCell (getColor name)) locations

colorMap :: Map Char String
colorMap = fromList [ ('F', "green")
                    , ('I', "blue")
                    , ('L', "red")
                    , ('N', "yellow")
                    , ('P', "purple")
                    , ('T', "brown")
                    , ('U', "maroon")
                    , ('V', "cyan")
                    , ('W', "pink")
                    , ('X', "orange")
                    , ('Y', "indigo")
                    , ('Z', "topaz")
                    ]

getColor :: Char -> String
getColor name = 
  case DM.lookup name colorMap of
    Just color -> color
    Nothing -> "black"  -- should not happen.

showCell :: String -> (Int, Int) -> Miso.View Action
showCell color (row,col) =
    MS.g_ [ MS.transform_
                (ms $    "scale (" ++ scale ++ ", " ++ scale ++ ") " 
                      ++ "translate (" ++ show col ++ ", " ++ show row ++ ") ")
          ]
          [ MS.rect_
                [ MS.x_ "0.05"
                , MS.y_ "0.05"
                , MS.width_ "0.9"
                , MS.height_ "0.9"
                , Miso.style_ $ fromList [("fill", ms color)]
                ]
                []
          ] 
    where scale = show cellSize

