{-# LANGUAGE OverloadedStrings #-}

module Interaction (initialState, handleEvent, drawWorld) where

import Data.Text
import Data.Vector qualified as Vec
import Prelude hiding (Left, Right)

import Board
import CodeWorld

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

getTile :: Coordinate -> MazeGrid -> Maybe Tile
getTile (Coordinate col row) grid = grid Vec.!? row >>= (Vec.!? col)

movePlayer :: Direction -> Maze -> Maze
movePlayer direction maze'@(Maze grid player) = 
  let targetCoordinate = trace "target coordinate" $ adjacentCoordinate direction (location player)
      newMaze = maze' { player = Player targetCoordinate direction } in
  case getTile targetCoordinate grid of
    Just (Tile Ground Nothing) -> newMaze
    Just (Tile Storage Nothing) -> newMaze
    -- cannot move into overlays (yet)
    _ -> Maze grid (player { direction = direction })

handleEvent :: Event -> Maze -> Maze
handleEvent (KeyPress key) = maybe id movePlayer (readMaybe key)
handleEvent _ = id

drawWorld :: Maze -> Picture
drawWorld = scaled 0.2 0.2 . translateBlock (Coordinate (-4) (-4)) . drawMaze

initialState :: Maze
initialState = maze
