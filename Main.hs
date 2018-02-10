{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Miso
import Miso.String

data Model = Model
  { time :: Double
  , delta :: Double
  } deriving (Show, Eq)

initialModel :: Model
initialModel =
  Model
  { time = 0
  , delta = 0
  }

main :: IO ()
main = do
  t <- now
  let m  = initialModel { time = t}
  startApp App { model = m
               , initialAction = Init
               , update = updateModel
               , view   = viewModel
               , events = defaultEvents
               , mountPoint = Nothing
               , subs   = []
               }

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel Init model@Model {..} = model <# (Time <$> now)
updateModel (Time newTime) model = 
  step $ model {delta = newTime - time model, time = newTime}

step :: Model -> Effect Action Model
step model@Model {..} = k <# (Time <$> now)
  where
    k = model

data Action
  = NoOp
  | Time Double
  | Init
  deriving (Show, Eq)

viewModel :: Model -> View Action
viewModel x = div_ [] [ text $ ms (show x) ]
