{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Board (
  Direction(..),
  EnterableBlock(..),
  OverlayBlock(..),
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

-- Coordinate storing a colNumber and rowNumber
data Coordinate = Coordinate Int Int deriving (Show, Eq)

data Direction = Up | Down | Left | Right deriving Show

data EnterableBlock = Ground | Storage deriving Show

data OverlayBlock = Box deriving Show

data Tile = Wall | EnterableTile EnterableBlock (Maybe OverlayBlock) deriving Show

data Player = Player {
  location :: Coordinate,
  direction :: Direction
} deriving Show

type MazeGrid = Vec.Vector (Vec.Vector Tile)

data Maze = Maze {
  grid :: MazeGrid,
  player :: Player
} deriving Show

-- move a block to the given coordinate
translateBlock :: Coordinate -> Picture -> Picture
translateBlock (Coordinate colNum rowNum) = translated (fromIntegral $ 4 * colNum) (fromIntegral $ 4 * rowNum)

drawBase :: EnterableBlock -> Picture
drawBase Ground = ground
drawBase Storage = storage

drawOverlay :: OverlayBlock -> Picture
drawOverlay Box = box

-- Draw a tile
drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile (EnterableTile base overlay) = maybe blank drawOverlay overlay & drawBase base

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
  | abs x == 4 || abs y == 4 = Just Wall
  | y == -2                  = Just $ EnterableTile Storage (Just Box)
  | x /= 0 && y == 1         = Just Wall
  | x == 0 && y == 1         = Just $ EnterableTile Ground (Just Box)
  | x == -3 && y == 2        = Just Wall
  | x == -2 && y == 2        = Just $ EnterableTile Storage Nothing
  | x == -1 && y == 2        = Just $ EnterableTile Ground (Just Box)
  | x == 1 && y == 3         = Just $ EnterableTile Storage Nothing
  | otherwise                = Just $ EnterableTile Ground Nothing

makeRow :: Int -> Vec.Vector Tile
makeRow rowNum = Vec.generate 9 (\colNum -> fromJust $ mazeTile $ Coordinate (colNum - 4) rowNum)

mazeGrid :: Vec.Vector (Vec.Vector Tile)
mazeGrid = Vec.generate 9 (\rowNum -> makeRow (rowNum - 4))

-- The initial maze state
maze :: Maze
maze = Maze mazeGrid (Player (Coordinate 1 1) Right)

drawRow :: Int -> Vec.Vector Tile -> Vec.Vector Picture
drawRow rowNum = Vec.imap (\colNum tile -> translateBlock (Coordinate colNum rowNum) (drawTile tile))

-- Draw the maze grid
drawGrid :: MazeGrid -> Picture
drawGrid = Vec.foldl (&) blank . Vec.concat' . Vec.imap drawRow

-- Draw the maze
drawMaze :: Maze -> Picture
drawMaze (Maze grid player) = drawPlayer player & drawGrid grid
