module VectorUtils where

import Data.Vector qualified as Vec
import Prelude hiding (concat)

iconcatMap :: (Int -> a -> b) -> Vec.Vector a -> Vec.Vector b
iconcatMap f = Vec.map (uncurry f) . Vec.indexed

concat' :: Foldable t => t (Vec.Vector a) -> Vec.Vector a
concat' = foldl (Vec.++) Vec.empty

replace :: Int -> a -> Vec.Vector a -> Vec.Vector a
replace index item vec = vec Vec.// [(index, item)]
