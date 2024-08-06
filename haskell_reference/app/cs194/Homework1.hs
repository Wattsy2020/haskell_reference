module Homework1 where

import CodeWorld

-- exercise 2
angle :: Double
angle = pi / 10

tree :: Integer -> Picture -> Picture
tree 0 bloomPicture = bloomPicture
tree n bloomPicture = polyline [(0,0),(0,1)] & translated 0 1 (
    rotated angle (tree (n-1) bloomPicture) & rotated (-angle) (tree (n-1) bloomPicture))

bloom :: Double -> Picture
bloom size = colored yellow $ solidCircle size

bloomSize :: Double -> Double -> Double
bloomSize growUntilTime currentTime = (min currentTime growUntilTime / growUntilTime) / 4

treeAnimation :: Integer -> Double -> Picture
treeAnimation depth time = tree depth $ bloom (bloomSize 10 time)

homework1Main :: IO ()
homework1Main = animationOf $ treeAnimation 8
