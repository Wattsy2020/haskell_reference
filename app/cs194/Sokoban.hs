module Sokoban where

import CodeWorld
import Data.Maybe (fromJust)

data Block = Wall | Ground | Storage | Box

-- A picture of a solid rectangle with an outline
solidThickRectangle :: Color -> Color -> Double -> Double -> Double -> Picture
solidThickRectangle borderColor fillColor borderWidth width height =
    colored borderColor (thickRectangle borderWidth width height)
    & colored fillColor (solidRectangle width height)

-- A picture of half a brick
halfBrick :: Picture
halfBrick =
    -- color three sides with a black border
    -- and the other side with a border the same color as the fill, so that we get an even square
    colored black border & colored brown (sideFill & fill)
    where
        -- 0.55s are to extend the black border slightly, so that it reaches the end of the half brick
        border = thickPolyline 0.1 [(0.55, -0.5), (-0.5, -0.5), (-0.5, 0.5), (0.55, 0.5)]
        sideFill = thickPolyline 0.1 [(0.5, -0.5), (0.5, 0.5)]
        fill = solidRectangle 1 1

-- A picture of a full brick
brick :: Picture
brick = translated (-0.5) 0 halfBrick & translated 0.5 0 (rotated pi halfBrick)

-- A picture of a row of bricks: either two full bricks or 1 full brick flanked by two half bricks
wallRow :: Int -> Picture
wallRow rowNum
    | even rowNum = translated (-1) 0 brick & translated 1 0 brick
    | otherwise = translated (-1.5) 0 (rotated pi halfBrick) & brick & translated 1.5 0 halfBrick

-- A picture of a wall
wall :: Picture
wall = foldl (\pic rowNum -> pic & translated 0 (fromIntegral rowNum - 0.5) (wallRow rowNum)) blank [-1..2]

-- A picture of the ground
ground :: Picture
ground = colored gray $ solidRectangle 4 4

-- A picture of a storage space on the ground
storage :: Picture
storage = colored (light red) (solidCircle 1) & ground

-- A picture of a box
box :: Picture
box = colored (light brown) $ solidRectangle 3.5 3.5

-- Get the picture for the given block type
toPicture :: Block -> Picture
toPicture Wall = wall
toPicture Ground = ground
toPicture Storage = storage
toPicture Box = box

-- move a block given the number of blocks in the x and y directions to move it in
translateBlock :: Integer -> Integer -> Picture -> Picture
translateBlock colNum rowNum = translated (fromInteger $ 4 * colNum) (fromInteger $ 4 * rowNum)

-- Define the maze by outlining which block goes in each row and column
maze :: Integer -> Integer -> Maybe Block
maze x y
  | abs x > 4  || abs y > 4  = Nothing
  | abs x == 4 || abs y == 4 = Just Wall
  | x ==  2 && y <= 0        = Just Wall
  | x ==  3 && y <= 0        = Just Storage
  | x >= -2 && y == 0        = Just Box
  | otherwise                = Just Ground

-- Add a maze block to the existing maze
addMazeBlock :: Picture -> Integer -> Integer -> Picture
addMazeBlock currentMaze colNum rowNum =
    let picture = toPicture $ fromJust $ maze colNum rowNum in
        currentMaze & translateBlock colNum rowNum picture

-- Draw a row of the maze
mazeRow :: Integer -> Picture
mazeRow rowNum = foldl (\picture colNum -> addMazeBlock picture colNum rowNum) blank [(-4)..4]

-- A picture of the full maze
mazePicture :: Picture
mazePicture = foldl (\picture rowNum -> picture & mazeRow rowNum) blank [(-4)..4]

-- The game board
board :: Picture
board = scaled 0.2 0.2 mazePicture

playSokoban :: IO ()
playSokoban = drawingOf board
