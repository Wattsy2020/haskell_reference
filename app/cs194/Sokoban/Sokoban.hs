module Sokoban where

import CodeWorld
import Interaction
import Board

playSokoban :: IO ()
playSokoban = drawingOf $ drawMaze maze -- activityOf initialState handleEvent drawWorld
