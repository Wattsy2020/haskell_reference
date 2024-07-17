module Board where

import CodeWorld
import Assets

data Coordinate = Coordinate Integer Integer -- colNum, rowNum

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
