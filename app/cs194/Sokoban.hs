module Sokoban where

import CodeWorld

data Block = Wall | Ground | Storage | Box
data Coordinate = Coordinate Integer Integer -- colNum, rowNum

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

-- move a block to the given coordinate
translateBlock :: Coordinate -> Picture -> Picture
translateBlock (Coordinate colNum rowNum) = translated (fromInteger $ 4 * colNum) (fromInteger $ 4 * rowNum)

-- Define the grid the maze is drawn on
grid :: [[Coordinate]]
grid = map (\colNum -> map (Coordinate colNum) [(-4)..4]) [(-4)..4]

-- Define the maze by outlining which block goes in each row and column
maze :: Coordinate -> Maybe Block
maze (Coordinate x y)
  | abs x > 4  || abs y > 4  = Nothing
  | abs x == 4 || abs y == 4 = Just Wall
  | x ==  2 && y <= 0        = Just Wall
  | x ==  3 && y <= 0        = Just Storage
  | x >= -2 && y == 0        = Just Box
  | otherwise                = Just Ground

-- Add a maze block to the existing maze
addMazeBlock :: Picture -> Coordinate -> Picture
addMazeBlock currentMaze coordinate =
    case maze coordinate of
        Just blockType -> currentMaze & translateBlock coordinate (toPicture blockType)
        Nothing -> currentMaze

-- A picture of the full maze
mazePicture :: Picture
mazePicture = foldl addMazeBlock blank $ concat grid

-- The game board
board :: Picture
board = scaled 0.2 0.2 mazePicture

playSokoban :: IO ()
playSokoban = drawingOf board
