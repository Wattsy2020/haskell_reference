{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Board (
  Direction(..),
  BaseBlock(..),
  Coordinate(..),
  Tile(..),
  MazeGrid,
  Player(..),
  Maze(..),
  maze,
  drawMaze,
  translateBlock
) where

import Data.Vector qualified as Vec
import VectorUtils qualified as Vec
import Data.Maybe
import Prelude hiding (Left, Right)

import CodeWorld
import Assets
    ( box,
      ground,
      storage,
      wall,
      playerLeft,
      playerRight,
      playerUp,
      playerDown )

data Direction = Up | Down | Left | Right

data BaseBlock = Wall | Ground | Storage

data OverlayBlock = Box

data Coordinate = Coordinate Int Int -- colNum, rowNum

data Tile = Tile {
  baseBlock :: BaseBlock,
  overlayBlock :: Maybe OverlayBlock
}

data Player = Player {
  location :: Coordinate,
  direction :: Direction
}

type MazeGrid = Vec.Vector (Vec.Vector Tile)

data Maze = Maze {
  grid :: MazeGrid,
  player :: Player
}

-- move a block to the given coordinate
translateBlock :: Coordinate -> Picture -> Picture
translateBlock (Coordinate colNum rowNum) = translated (fromIntegral $ 4 * colNum) (fromIntegral $ 4 * rowNum)

-- draw a base block
drawBase :: BaseBlock -> Picture
drawBase Wall = wall
drawBase Ground = ground
drawBase Storage = storage

-- draw an overlay
drawOverlay :: OverlayBlock -> Picture
drawOverlay Box = box

-- draw the player
drawPlayer :: Player -> Picture
drawPlayer (Player location direction) = translateBlock location $ case direction of
  Left -> playerLeft
  Right -> playerRight
  Up -> playerUp
  Down -> playerDown

-- Define the maze by outlining which block goes in each row and column
mazeTile :: Coordinate -> Maybe Tile
mazeTile (Coordinate x y)
  | abs x > 4  || abs y > 4  = Nothing
  | abs x == 4 || abs y == 4 = Just $ Tile Wall Nothing
  | y == -2                  = Just $ Tile Storage (Just Box)
  | x /= 0 && y == 1         = Just $ Tile Wall Nothing
  | x == 0 && y == 1         = Just $ Tile Ground (Just Box)
  | x == -3 && y == 2        = Just $ Tile Wall Nothing
  | x == -2 && y == 2        = Just $ Tile Storage Nothing
  | x == -1 && y == 2        = Just $ Tile Ground (Just Box)
  | x == 1 && y == 3         = Just $ Tile Storage Nothing
  | otherwise                = Just $ Tile Ground Nothing

makeRow :: Int -> Vec.Vector Tile
makeRow rowNum = Vec.generate 9 (\colNum -> fromJust $ mazeTile $ Coordinate (colNum - 4) rowNum)

mazeGrid :: Vec.Vector (Vec.Vector Tile)
mazeGrid = Vec.generate 9 (\rowNum -> makeRow (rowNum - 4))

-- The initial maze state
maze :: Maze
maze = Maze mazeGrid (Player (Coordinate 1 1) Right)

-- Draw a tile
drawTile :: Tile -> Picture
drawTile (Tile base overlay) = maybe blank drawOverlay overlay & drawBase base

drawRow :: Int -> Vec.Vector Tile -> Vec.Vector Picture
drawRow rowNum = Vec.imap (\colNum tile -> translateBlock (Coordinate colNum rowNum) (drawTile tile))

-- Draw the maze grid
drawGrid :: MazeGrid -> Picture
drawGrid = Vec.foldl (&) blank . Vec.concat' . Vec.imap drawRow

-- Draw the maze
drawMaze :: Maze -> Picture
drawMaze (Maze grid player) = drawPlayer player & drawGrid grid
