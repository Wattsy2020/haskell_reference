{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <=<" #-}

module Interaction (gameLoop) where

import Data.Text
import Prelude hiding (Left, Right)
import CodeWorld

import Board
import Assets (startScreen, wonScreen)
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

data Interaction state = Interaction {
    initialStateF :: state, 
    handleEventF :: Event -> state -> state,
    drawWorldF :: state -> Picture
}

data StartableGameState state = Starting | Running state

startableInteraction :: forall state. Interaction state -> Interaction (StartableGameState state)
startableInteraction Interaction{..} = Interaction Starting handleEvent' drawWorld'
    where
        handleEvent' :: Event -> StartableGameState state -> StartableGameState state
        handleEvent' (KeyPress " ") Starting = Running initialStateF
        handleEvent' _ Starting = Starting
        handleEvent' event (Running state) = Running $ handleEventF event state

        drawWorld' :: StartableGameState state -> Picture
        drawWorld' Starting = startScreen
        drawWorld' (Running world) = drawWorldF world

data WinnableGameState state = Won | Playing state deriving (Show, Eq)

winnableInteraction :: forall state. 
    (state -> Bool) 
    -> Interaction state 
    -> Interaction (WinnableGameState state)
winnableInteraction isWonF Interaction{..} = Interaction (Playing initialStateF) handleEvent' drawWorld'
    where
        handleEvent' :: Event -> WinnableGameState state -> WinnableGameState state
        handleEvent' _ Won = Won
        handleEvent' event (Playing state)
            | isWonF state = Won
            | otherwise = Playing $ handleEventF event state

        drawWorld' :: WinnableGameState state -> Picture
        drawWorld' Won = wonScreen
        drawWorld' (Playing state) = drawWorldF state

newtype UndoableState state = UndoableState [state] deriving Show

undoableInteraction :: forall state. (Eq state) => Interaction state -> Interaction (UndoableState state)
undoableInteraction Interaction{..} = Interaction (UndoableState [initialStateF]) handleEvent' drawWorld'
    where
        handleEvent' :: Event -> UndoableState state -> UndoableState state
        handleEvent' _ (UndoableState []) = error "can't handle an event with no state"
        handleEvent' (KeyPress "Z") firstState@(UndoableState [_]) = firstState
        handleEvent' (KeyPress "Z") (UndoableState (_ : previous)) = UndoableState previous
        handleEvent' event (UndoableState states@(mostRecent : _)) = 
            let newState = handleEventF event mostRecent in
                if newState == mostRecent 
                    then UndoableState states 
                    else UndoableState (newState : states)

        drawWorld' :: UndoableState state -> Picture
        drawWorld' (UndoableState (mostRecent : _)) = drawWorldF mostRecent
        drawWorld' (UndoableState []) = error "can't draw undrawable state"

resettableInteraction :: forall state. Interaction state -> Interaction state
resettableInteraction Interaction{..} = Interaction initialStateF handleEvent' drawWorldF    
    where
        handleEvent' :: Event -> state -> state
        handleEvent' (KeyPress "Esc") = const initialStateF
        handleEvent' event = handleEventF event

interactionOf :: Interaction a -> IO ()
interactionOf (Interaction initialStateF handleEventF drawWorldF) = 
    activityOf initialStateF handleEventF drawWorldF

gameLoop :: IO ()
gameLoop = interactionOf 
    $ resettableInteraction 
    $ startableInteraction 
    $ undoableInteraction
    $ winnableInteraction isWon
    $ Interaction maze handleEvent drawWorld
