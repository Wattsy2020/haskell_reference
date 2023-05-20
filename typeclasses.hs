import Data.List qualified as List
import Data.Set qualified as Set

-- a simple typeclass that allows us to check whether an element is contained by a Container
class Container f a where
  contains :: f a -> a -> Bool

instance (Eq a) => Container [] a where
  contains :: [a] -> a -> Bool
  contains = flip List.elem

instance (Ord a) => Container Set.Set a where
  contains :: Set.Set a -> a -> Bool
  contains = flip Set.member

-- TODO: figure out how to make a function an element of Container
-- data ContainerLambda a = ContainerLambda (a -> Bool)
-- instance Container ContainerLambda a where

main = do
  let list = [5 .. 10]
  let set = Set.fromList [1, 2, 3, 4, 5]
  print (contains set 1, contains set 10)
  print (contains list 1, contains list 10)