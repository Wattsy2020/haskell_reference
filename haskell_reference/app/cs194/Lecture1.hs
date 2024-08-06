{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lecture1 (lecture1Main) where

import CodeWorld

ourPicture :: Picture
ourPicture = solidCircle 1

composedPicture :: Picture
composedPicture = colored green (solidCircle 1) & solidCircle 2

solidThickRectangle :: Color -> Color -> Double -> Double -> Double -> Picture
solidThickRectangle borderColor fillColor borderWidth width height = 
    colored borderColor (thickRectangle borderWidth width height)
    & colored fillColor (solidRectangle width height)

tetrisBlock :: Picture
tetrisBlock = 
    let rectangle' = solidThickRectangle black green 0.1 1 1 in
    translated (-1) 0 rectangle' 
    & translated 0 1 rectangle'
    & translated 1 0 rectangle'
    & rectangle'

truncateDouble :: Double -> Double
truncateDouble = fromInteger . truncate

fallingBlock :: Double -> Picture
fallingBlock time = translated 0 (- truncateDouble time) tetrisBlock

tree :: Integer -> Double -> Picture
tree 0 _ = blank
tree n angle = polyline [(0,0),(0,1)] & translated 0 1 (
    rotated angle (tree (n-1) angle) & rotated (-angle) (tree (n-1) angle))

treeFolding :: Integer -> Double -> Picture
treeFolding depth time = 
    let normalisedTime = time - truncateDouble time
        angleMultipler = abs (normalisedTime - 0.5) / 0.5 in
        tree depth (pi * angleMultipler / 10)

scaleAnimationTime :: Double -> (Double -> Picture) -> (Double -> Picture)
scaleAnimationTime scale f = f . (scale*)

lecture1Main :: IO ()
lecture1Main = animationOf $ scaleAnimationTime 0.1 $ treeFolding 8 
--animationOf fallingBlock
