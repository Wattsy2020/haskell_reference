module Sokoban where

import CodeWorld
import Interaction

playSokoban :: IO ()
playSokoban = activityOf initialState handleEvent drawWorld
