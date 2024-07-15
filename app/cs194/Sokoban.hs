module Sokoban where

import CodeWorld

data Block = Wall | Ground | Storage | Box

solidThickRectangle :: Color -> Color -> Double -> Double -> Double -> Picture
solidThickRectangle borderColor fillColor borderWidth width height =
    colored borderColor (thickRectangle borderWidth width height)
    & colored fillColor (solidRectangle width height)

halfBrick :: Picture
halfBrick =
    let border = thickPolyline 0.1 [(0.5, -0.5), (-0.5, -0.5), (-0.5, 0.5), (0.5, 0.5)] in
    let fill = solidRectangle 1 1 in
        colored black border & colored brown fill

brick :: Picture
brick = solidThickRectangle black brown 0.1 2 1

wallRow :: Int -> Picture
wallRow rowNum
    | even rowNum = translated (-1) 0 brick & translated 1 0 brick
    | otherwise = translated (-1.5) 0 (rotated pi halfBrick) & brick & translated 1.5 0 halfBrick

wall :: Picture
wall = foldl (\pic rowNum -> pic & translated 0 (fromIntegral (-rowNum)) (wallRow rowNum)) blank [0, 1, 2, 3]

ground :: Picture
ground = colored gray $ solidRectangle 4 4

storage :: Picture
storage = ground & colored red $ solidCircle 1

box :: Picture
box = colored brown $ solidRectangle 3.5 3.5

maze :: Integer -> Integer -> Maybe Block
maze x y
  | abs x > 4  || abs y > 4  = Nothing
  | abs x == 4 || abs y == 4 = Just Wall
  | x ==  2 && y <= 0        = Just Wall
  | x ==  3 && y <= 0        = Just Storage
  | x >= -2 && y == 0        = Just Box
  | otherwise                = Just Ground

playSokoban :: IO ()
playSokoban = drawingOf (wall & translated 2 0 wall) -- (halfBrick & translated 0 1 brick)