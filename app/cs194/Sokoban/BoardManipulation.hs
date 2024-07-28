{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <=<" #-}

module BoardManipulation (movePlayer) where

import Data.Vector qualified as Vec
import VectorUtils qualified as Vec
import Data.Maybe
import Prelude hiding (Left, Right)
import Board

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
