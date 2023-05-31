import Data.Function (on)
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

newtype LambdaContainer a = LambdaContainer (a -> Bool)

instance Container LambdaContainer where
  contains :: LambdaContainer a -> a -> Bool
  contains (LambdaContainer f) = f

union :: (Container f1, Container f2, Ord a) => f1 a -> f2 a -> LambdaContainer a
union c1 c2 = LambdaContainer (\x -> contains c1 x && contains c2 x)

allContainer :: LambdaContainer a
allContainer = LambdaContainer (const True)

toLambda :: (Container f, Ord a) => f a -> LambdaContainer a
toLambda container = LambdaContainer (contains container)

main = do
  let list = [4 .. 10]
  let set = Set.fromList [1, 2, 3, 4, 5]
  let evenLambda = LambdaContainer even
  print (contains set 1, contains set 10, contains evenLambda 4)
  print (contains list 1, contains list 10, contains evenLambda 5)
  print $ filter (contains $ union list set) [1 .. 10]
  print $ filter (contains $ union set evenLambda) [1 .. 10]
  let containers = [toLambda list, toLambda set, evenLambda]
  let unionContainer = foldl1 union containers
  print $ filter (contains unionContainer) [1 .. 10]