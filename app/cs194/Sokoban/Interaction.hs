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
handleEvent (KeyPress key) = maybe id movePlayer (readMaybe key)
handleEvent _ = id

drawWorld :: Maze -> Picture
drawWorld = scaled 0.2 0.2 . translateBlock (Coordinate (-4) (-4)) . drawMaze

data Interaction state = Interaction state (Event -> state -> state) (state -> Picture)

data StartableGameState a = Starting | Running a

startableInteraction :: forall state. Interaction state -> Interaction (StartableGameState state)
startableInteraction (Interaction initialStateF handleEventF drawWorldF) = 
    Interaction Starting handleEvent' drawWorld'
    where
        handleEvent' :: Event -> StartableGameState state -> StartableGameState state
        handleEvent' (KeyPress " ") Starting = Running initialStateF
        handleEvent' _ Starting = Starting
        handleEvent' event (Running state) = Running $ handleEventF event state

        drawWorld' :: StartableGameState state -> Picture
        drawWorld' Starting = startScreen
        drawWorld' (Running world) = drawWorldF world

resettableInteraction :: forall state. Interaction state -> Interaction state
resettableInteraction (Interaction initialStateF handleEventF drawWorldF) =
    Interaction initialStateF handleEvent' drawWorldF    
    where
        handleEvent' :: Event -> state -> state
        handleEvent' (KeyPress "Esc") = const initialStateF
        handleEvent' event = handleEventF event

interactionOf :: Interaction a -> IO ()
interactionOf (Interaction initialStateF handleEventF drawWorldF) = 
    activityOf initialStateF handleEventF drawWorldF

-- todo: implement the "Won the game" screen
-- then we want gameLoop = resetable ( winnable ( startable ( baseInteraction )))

gameLoop :: IO ()
gameLoop = interactionOf 
    $ resettableInteraction 
    $ startableInteraction 
    $ Interaction maze handleEvent drawWorld
