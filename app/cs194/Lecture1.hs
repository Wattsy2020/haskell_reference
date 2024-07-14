module Lecture1 (lecture1Main) where

import CodeWorld

ourPicture :: Picture
ourPicture = solidCircle 1

lecture1Main :: IO ()
lecture1Main = drawingOf ourPicture