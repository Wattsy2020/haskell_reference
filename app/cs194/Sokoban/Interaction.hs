{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <=<" #-}

module Interaction (gameLoop) where

import Data.Text
import Prelude hiding (Left, Right)
import CodeWorld

import Board
import Assets (startScreen)
import BoardManipulation (movePlayer)

-- Read text into a direction
readMaybe :: Text -> Maybe Direction
readMaybe "Up" = Just Up
readMaybe "Down" = Just Down
readMaybe "Left" = Just Left
readMaybe "Right" = Just Right
readMaybe _ = Nothing

handleEvent :: Event -> Maze -> Maze
handleEvent (KeyPress "Esc") = const maze
handleEvent (KeyPress key) = maybe id movePlayer (readMaybe key)
handleEvent _ = id

drawWorld :: Maze -> Picture
drawWorld = scaled 0.2 0.2 . translateBlock (Coordinate (-4) (-4)) . drawMaze

data StartableGameState a = Starting | Running a

startableActivityOf :: forall world . world -> (Event -> world -> world) -> (world -> Picture) -> IO ()
startableActivityOf initialStateF handleEventF drawWorldF = activityOf Starting handleEvent' drawWorld'
    where
        handleEvent' :: Event -> StartableGameState world -> StartableGameState world
        handleEvent' (KeyPress " ") Starting = Running initialStateF
        handleEvent' _ Starting = Starting
        handleEvent' event (Running state) = Running $ handleEventF event state

        drawWorld' :: StartableGameState world -> Picture
        drawWorld' Starting = startScreen
        drawWorld' (Running world) = drawWorldF world

-- todo: implement the "Won the game" screen
-- we can use the `data Interaction` concept from the lecture 
-- and define a new function that produces an interaction of a winnable game, 
-- showing a game over screen when a condition is met
-- then we want gameLoop = resetable ( winnable ( startable ( baseInteraction )))

gameLoop :: IO ()
gameLoop = startableActivityOf maze handleEvent drawWorld
