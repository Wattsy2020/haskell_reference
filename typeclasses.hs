import Data.List qualified as List
import Data.Set qualified as Set

-- a typeclass that allows us to check whether an element is contained by a Container
-- to be realistic in implementation, we need to have all elements in the container be Ord A
class Container f where
  contains :: Ord a => f a -> a -> Bool

instance Container [] where
  contains :: Eq a => [a] -> a -> Bool
  contains = flip List.elem

instance Container Set.Set where
  contains :: Ord a => Set.Set a -> a -> Bool
  contains = flip Set.member

-- todo: figure out how to implement this for all functions that return a bool, e.g. (a -> Bool)
-- for some reason it's hard to write that as a constraint

main = do
  let list = [5 .. 10]
  let set = Set.fromList [1, 2, 3, 4, 5]
  print (contains set 1, contains set 10)
  print (contains list 1, contains list 10)