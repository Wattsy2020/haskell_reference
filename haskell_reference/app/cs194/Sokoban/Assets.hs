{-# LANGUAGE OverloadedStrings #-}

module Assets (
    wall, 
    ground, 
    storage, 
    box, 
    playerLeft,
    playerRight,
    playerUp,
    playerDown,
    startScreen,
    wonScreen
) where 

import CodeWorld
import Data.Text ()

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

-- Returns a picture of a rectangular body part given width and height
bodyPart :: Double -> Double -> Picture
bodyPart = solidThickRectangle black (light pink) 0.1

-- A picture of the player facing left
playerLeft :: Picture
playerLeft = head' & leftArm & rightArm & torso & leftLeg & rightLeg
    where
        leg = bodyPart 0.4 1.5
        leftLeg = translated (-0.6) (-1) $ rotated (- (pi / 10)) leg
        rightLeg = translated 0.6 (-1) $ rotated (pi / 10) leg
        arm = bodyPart 0.3 1.2
        leftArm = translated (-0.7) 0.2 $ rotated (- (pi / 3)) arm
        rightArm = translated 0.6 0 $ rotated (pi / 10) arm
        torso = bodyPart 0.8 1
        head' = translated 0 0.9 $ bodyPart 0.5 0.5

playerRight :: Picture
playerRight = reflected (pi/2) playerLeft

playerUp :: Picture 
playerUp = head' & shoulders & leftArm & rightArm
    where 
        head' = bodyPart 0.6 0.7
        shoulders = bodyPart 1.5 0.6
        leftArm = translated (-0.7) 0.6 $ rotated (pi/10) $ bodyPart 0.4 0.6
        rightArm = translated 0.7 0.4 $ rotated (-(pi/10)) $ bodyPart 0.4 0.2

playerDown :: Picture
playerDown = rotated pi playerUp

startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban!") 
    & scaled 0.5 0.5 $ translated 0 (-4) $ lettering "Press Space to Start"

wonScreen :: Picture
wonScreen = scaled 3 3 (lettering "You Won!") 
    & scaled 0.5 0.5 $ translated 0 (-4) $ lettering "Thanks for Playing :)"
