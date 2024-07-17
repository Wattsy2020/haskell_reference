{-# LANGUAGE OverloadedStrings #-}

module Interaction (initialState, handleEvent, drawWorld) where

import Board
import CodeWorld
import Data.Text
import Prelude hiding (Left, Right)

data Direction = Up | Down | Left | Right

readMaybe :: Text -> Maybe Direction
readMaybe "Up" = Just Up
readMaybe "Down" = Just Down
readMaybe "Left" = Just Left
readMaybe "Right" = Just Right
readMaybe _ = Nothing

adjacentCoordinate :: Direction -> Coordinate -> Coordinate
adjacentCoordinate Up (Coordinate x y) = Coordinate x (y + 1)
adjacentCoordinate Down (Coordinate x y) = Coordinate x (y - 1)
adjacentCoordinate Left (Coordinate x y) = Coordinate (x - 1) y
adjacentCoordinate Right (Coordinate x y) = Coordinate (x + 1) y

handleEvent :: Event -> Coordinate -> Coordinate
handleEvent (KeyPress key) coordinate = case readMaybe key of
    Just direction -> adjacentCoordinate direction coordinate
    Nothing -> coordinate
handleEvent _ coordinate = coordinate

drawWorld :: Coordinate -> Picture
drawWorld coordinate = scaled 0.2 0.2 $ translateBlock coordinate mazePicture

initialState :: Coordinate
initialState = Coordinate 0 0
