{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}

import Data.Function (on)
import Data.List qualified as List
import Data.Set qualified as Set

-- a typeclass that allows us to check whether an element is contained by a Container
class Container f a where
  contains :: f a -> a -> Bool

instance Eq a => Container [] a where
  contains :: [a] -> a -> Bool
  contains = flip List.elem

instance Ord a => Container Set.Set a where
  contains :: Set.Set a -> a -> Bool
  contains = flip Set.member

newtype LambdaContainer a = LambdaContainer (a -> Bool)

instance Container LambdaContainer a where
  contains :: LambdaContainer a -> a -> Bool
  contains (LambdaContainer f) = f

union :: (Container f1 a, Container f2 a) => f1 a -> f2 a -> LambdaContainer a
union c1 c2 = LambdaContainer (\x -> contains c1 x && contains c2 x)

allContainer :: LambdaContainer a
allContainer = LambdaContainer (const True)

toLambda :: (Container f a) => f a -> LambdaContainer a
toLambda container = LambdaContainer (contains container)

{-
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
-}

class CyclicEnum a where
  pred' :: a -> a
  succ' :: a -> a

  default pred' :: (Bounded a, Enum a, Eq a) => a -> a
  pred' x
    | x == (minBound :: a) = maxBound :: a
    | otherwise = pred x

  default succ' :: (Bounded a, Enum a, Eq a) => a -> a
  succ' x
    | x == (maxBound :: a) = minBound :: a
    | otherwise = succ x

data Ring = Zero | One | Two | Three | Four | Five deriving (Show, Eq, Enum, Bounded, CyclicEnum)

instance Num Ring where
  (+) :: Ring -> Ring -> Ring
  (+) r1 Zero = r1
  (+) r1 r2 = succ' r1 + pred r2

  -- the inverse of the number x is y, where x + y = Zero (i.e. the additive identity)
  negate :: Ring -> Ring
  negate Zero = Zero
  negate x = succ $ negate (succ' x)

  (*) :: Ring -> Ring -> Ring
  (*) r1 Zero = Zero
  (*) r1 r2 = r1 + (r1 * pred r2)

  abs :: Ring -> Ring
  abs = id

  signum :: Ring -> Ring
  signum = const One

  fromInteger :: Integer -> Ring
  fromInteger 0 = Zero
  fromInteger x = succ' $ fromInteger (x - 1)

main = do
  print (succ' Two, succ' Five, pred' Zero)
  print (One + One, Four + Four, Five + 2)
  print (negate One, negate Two, negate Five)
  print (Two - One, Five - Four, Five - Three, One - One)
  print (Two * Two, 2 * Five)