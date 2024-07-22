module Board (
  Direction(..),
  BaseBlock(..),
  OverlayBlock(..),
  Coordinate(..),
  Tile(..),
  maze,
  drawMaze,
) where

import Data.Vector qualified as Vec
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

data OverlayBlock = Box | Player Direction

data Coordinate = Coordinate Int Int -- colNum, rowNum

data Tile = Tile {
  baseBlock :: BaseBlock,
  overlayBlock :: Maybe OverlayBlock
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
drawOverlay (Player Left) = playerLeft
drawOverlay (Player Right) = playerRight
drawOverlay (Player Up) = playerUp
drawOverlay (Player Down) = playerDown

-- Define the maze by outlining which block goes in each row and column
mazeTile :: Coordinate -> Maybe Tile
mazeTile (Coordinate x y)
  | abs x > 4  || abs y > 4  = Nothing
  | abs x == 4 || abs y == 4 = Just $ Tile Wall Nothing
  | x == -3 && y == -3       = Just $ Tile Ground (Just $ Player Right)
  | x ==  2 && y <= 0        = Just $ Tile Wall Nothing
  | x ==  3 && y <= 0        = Just $ Tile Storage Nothing
  | x >= -2 && y == 0        = Just $ Tile Ground (Just Box)
  | otherwise                = Just $ Tile Ground Nothing

makeRow :: Int -> Vec.Vector Tile
makeRow rowNum = Vec.generate 9 (\colNum -> fromJust $ mazeTile $ Coordinate (colNum - 4) rowNum)

-- The initial maze state
maze :: Vec.Vector (Vec.Vector Tile)
maze = Vec.generate 9 (\rowNum -> makeRow (rowNum - 4))

-- Draw a tile
drawTile :: Tile -> Picture
drawTile (Tile base overlay) = maybe blank drawOverlay overlay & drawBase base

drawRow :: Int -> Vec.Vector Tile -> Picture
drawRow rowNum = Vec.ifoldl fold' blank
  where
    fold' :: Picture -> Int -> Tile -> Picture
    fold' picture colNum tile = picture & translateBlock (Coordinate colNum rowNum) (drawTile tile)

-- Draw the maze
drawMaze :: Vec.Vector (Vec.Vector Tile) -> Picture
drawMaze = Vec.ifoldl (\picture rowNum -> (picture &) . drawRow rowNum) blank 
