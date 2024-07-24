{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <=<" #-}

module Interaction (gameLoop) where

import Data.Text
import Data.Vector qualified as Vec
import VectorUtils qualified as Vec
import Data.Maybe
import Prelude hiding (Left, Right)
import CodeWorld

import Board
import Assets (startScreen)

-- Read text into a direction
readMaybe :: Text -> Maybe Direction
readMaybe "Up" = Just Up
readMaybe "Down" = Just Down
readMaybe "Left" = Just Left
readMaybe "Right" = Just Right
readMaybe _ = Nothing

-- Move a coordinate in a given direction
adjacentCoordinate :: Direction -> Coordinate -> Coordinate
adjacentCoordinate Up (Coordinate x y) = Coordinate x (y + 1)
adjacentCoordinate Down (Coordinate x y) = Coordinate x (y - 1)
adjacentCoordinate Left (Coordinate x y) = Coordinate (x - 1) y
adjacentCoordinate Right (Coordinate x y) = Coordinate (x + 1) y

-- Try to get the tile at a coordinate (fails if tile is outside the maze)
getTile :: Coordinate -> MazeGrid -> Maybe Tile
getTile (Coordinate col row) grid = grid Vec.!? row >>= (Vec.!? col)

-- Replace a coordinate with the given tile
-- fails if trying to update the player tile
-- no update is performed if the coordinate is outside the maze
updateCoordinate :: Coordinate -> Tile -> Maze -> Maybe Maze
updateCoordinate coord@(Coordinate col row) tile (Maze grid player)
    | coord == location player  = Nothing
    | otherwise                 = Just $ Maze (Vec.replace row (Vec.replace col tile (grid Vec.! row)) grid) player

-- Try to place a box in the coordinate 
-- fails if the coordinate cannot be replaced or is outside the maze
placeBox :: Coordinate -> Maze -> Maybe Maze
placeBox coordinate maze'@(Maze grid _) = case getTile coordinate grid of
    Just (EnterableTile blockType Nothing) -> updateCoordinate coordinate (EnterableTile blockType (Just Box)) maze'
    _ -> Nothing

-- Try to remove a box in a coordinate
-- fails if the coordinate does not have a box, is outside the maze
removeBox :: Coordinate -> Maze -> Maybe Maze
removeBox coordinate maze'@(Maze grid _) = case getTile coordinate grid of
    Just (EnterableTile blockType (Just Box)) -> updateCoordinate coordinate (EnterableTile blockType Nothing) maze'
    _ -> Nothing

tryMoveBox :: Direction -> Coordinate -> Maze -> Maybe Maze
tryMoveBox direction coordinate =
    let targetCoordinate = adjacentCoordinate direction coordinate in
        (>>= placeBox targetCoordinate) . removeBox coordinate

movePlayer :: Direction -> Maze -> Maze
movePlayer direction maze'@(Maze grid player) =
  let targetCoordinate = adjacentCoordinate direction (location player)
      newPlayer = Player targetCoordinate direction
      unmovedMaze = Maze grid (player { direction = direction }) in
  case getTile targetCoordinate grid of
    Just (EnterableTile _ Nothing) -> maze' { player = newPlayer }
    Just (EnterableTile _ (Just Box)) -> 
        maybe 
        unmovedMaze 
        (\maze1 -> maze1 { player = newPlayer }) 
        (tryMoveBox direction targetCoordinate maze')
    _ -> unmovedMaze

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
-- also split this into two modules: BoardManipulation (with MovePlayer and all dependencies) and Interaction

gameLoop :: IO ()
gameLoop = startableActivityOf maze handleEvent drawWorld
