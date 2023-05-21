import Data.Function (on)
import Data.List qualified as List
import Data.Map qualified as Map

data PeaNum = Succ PeaNum | Zero deriving (Show)

makePeaNum :: Int -> PeaNum
makePeaNum 0 = Zero
makePeaNum x = Succ $ makePeaNum $ x - 1

-- main = print (Zero, Succ Zero, makePeaNum 5)

data Complex a = Complex a a deriving (Show, Eq)

fromReal :: Num a => a -> Complex a
fromReal x = Complex x 0

conjugate :: Num a => Complex a -> Complex a
conjugate (Complex x y) = Complex x (-y)

modulus :: Floating a => Complex a -> a
modulus (Complex x y) = sqrt (x ^ 2 + y ^ 2)

norm :: Floating a => Complex a -> Complex a
norm c@(Complex x y) = let r = modulus c in Complex (x / r) (y / r)

instance (Ord a, Floating a) => Ord (Complex a) where
  compare :: (Ord a, Floating a) => Complex a -> Complex a -> Ordering
  compare = compare `on` modulus

instance (Floating a) => Num (Complex a) where
  (+) :: Num a => Complex a -> Complex a -> Complex a
  (+) (Complex x1 y1) (Complex x2 y2) = Complex (x1 + x2) (y1 + y2)

  (*) :: Num a => Complex a -> Complex a -> Complex a
  (*) (Complex x1 y1) (Complex x2 y2) = Complex (x1 * x2 - y1 * y2) (x1 * y2 + x2 * y1)

  negate :: Num a => Complex a -> Complex a
  negate (Complex x y) = Complex (negate x) (negate y)

  abs :: Floating a => Complex a -> Complex a
  abs = fromReal . modulus

  signum :: Floating a => Complex a -> Complex a
  signum = norm

  fromInteger :: Integer -> Complex a
  fromInteger x = Complex (fromInteger x) 0

instance (Floating a) => Fractional (Complex a) where
  (/) :: Floating a => Complex a -> Complex a -> Complex a
  (/) c1 c2@(Complex x2 y2) = numerator * Complex (1 / denominator) 0
    where
      numerator = c1 * conjugate c2
      denominator = (x2 ^ 2) + (y2 ^ 2)

  fromRational :: Rational -> Complex a
  fromRational x = Complex (fromRational x) 0

{-
main = do
  let c1 = Complex 1 1
  let c2 = Complex 2 2.5
  let i = Complex 0 1
  print (c1 + c2, c1 * c2, c1 * i, i ^ 2, i ^ 4)
  print (2 * c1, c1 - 1 == i)
  print (c1 < c2, c1 <= i, 2 * c2 > c2, i <= i, min c1 i)
  print $ map modulus [c1, c2, i]
  print $ map norm [c1, c2, i]
  print $ List.sort [c1, c2, i]
  print (sum [c1, c2, i], product [c1, c2, i])
  print (conjugate i, 1 / i, 1 / i == i ^ 3)
  print (c1 / 2, c1 / c2, c1 / i)
-}

data Fraction a = Fraction a a

simplify :: (Integral a) => Fraction a -> Fraction a
simplify (Fraction num denom) = Fraction (num `div` result) (denom `div` result)
  where
    result = gcd num denom

instance (Show a) => Show (Fraction a) where
  show :: Fraction a -> String
  show (Fraction num denom) = show num ++ "/" ++ show denom

instance (Integral a) => Eq (Fraction a) where
  (==) :: Fraction a -> Fraction a -> Bool
  (==) f1 f2 = (n1 == n2) && (d1 == d2)
    where
      (Fraction n1 d1) = simplify f1
      (Fraction n2 d2) = simplify f2

instance (Ord a, Integral a) => Ord (Fraction a) where
  compare :: Fraction a -> Fraction a -> Ordering
  compare (Fraction n1 d1) (Fraction n2 d2) = compare (n1 * d2) (n2 * d1)

instance (Integral a) => Num (Fraction a) where
  (+) :: Fraction a -> Fraction a -> Fraction a
  (+) (Fraction n1 d1) (Fraction n2 d2) = Fraction (n1 * d2 + n2 * d1) (d1 * d2)

  (*) :: Fraction a -> Fraction a -> Fraction a
  (*) (Fraction n1 d1) (Fraction n2 d2) = Fraction (n1 * n2) (d1 * d2)

  abs :: Fraction a -> Fraction a
  abs (Fraction num denom) = Fraction (abs num) (abs denom)

  signum :: Fraction a -> Fraction a
  signum (Fraction num denom) = Fraction (signum num * signum denom) 1

  negate :: Fraction a -> Fraction a
  negate (Fraction num denom) = Fraction (negate num) denom

  fromInteger :: Integer -> Fraction a
  fromInteger int = Fraction (fromInteger int) 1

fracRecip :: Fraction a -> Fraction a
fracRecip (Fraction num denom) = Fraction denom num

fracDiv :: Integral a => Fraction a -> Fraction a -> Fraction a
fracDiv f1 f2 = f1 * fracRecip f2

main = do
  let f1 = Fraction 1 2
  let f2 = Fraction 3 6
  let f3 = Fraction 4 7
  let f4 = Fraction 15 18
  print (f1, f2, f3, f4)
  print (simplify f1, simplify f2, simplify f3, simplify f4)
  print (f1, f2, f3, f1 == f2, f1 < f3, f2 < f3, f4 > f3)
  print (f1 + f1, 3 * f1, f1 * f1, f1 * f2, f2 + f1, -f1, f1 - f2, simplify $ f1 - f2)
  print (1 `fracDiv` f1, f2 `fracDiv` f2)

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

-- TODO: make it a self balancing Tree
insert :: Ord a => a -> Tree a -> Tree a
insert item Leaf = Node Leaf item Leaf
insert item (Node leftTree nodeVal rightTree)
  | item < nodeVal = Node (insert item leftTree) nodeVal rightTree
  | otherwise = Node leftTree nodeVal (insert item rightTree)

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Leaf

member :: Ord a => a -> Tree a -> Bool
member item Leaf = False
member item (Node leftTree nodeVal rightTree)
  | item == nodeVal = True
  | item < nodeVal = member item leftTree
  | otherwise = member item rightTree

-- TODO: make Tree foldable and implement these
inOrder :: Tree a -> [a]
inOrder Leaf = []
inOrder (Node leftTree nodeVal rightTree) = inOrder leftTree ++ nodeVal : inOrder rightTree

height :: Tree a -> Int
height Leaf = 0
height (Node leftTree _ rightTree) = 1 + max (height leftTree) (height rightTree)

treeLength :: Tree a -> Int
treeLength = length . inOrder

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f Leaf = Leaf
  fmap f (Node leftTree nodeVal rightTree) = Node (fmap f leftTree) (f nodeVal) (fmap f rightTree)

{-
main = do
  let tree = fromList [9, 3, 1, 5, 8, 6]
  print (tree, member 5 tree, member 10 tree, inOrder tree, height tree, treeLength tree)
  print (fmap (* 2) tree, 0 <$ tree)
-}

-- instance Functor (Map.Map k) where
fmap' :: Ord k => (v -> v') -> Map.Map k v -> Map.Map k v'
fmap' f hashMap
  | Map.null hashMap = Map.empty
  | otherwise = Map.fromList mapped
  where
    kvs = Map.toList hashMap
    mapped = map (fmap f) kvs -- fmap over the tuples

-- define Functor for a 5 element tuple
instance Functor ((,,,,) a b c d) where
  fmap :: (e -> f) -> (a, b, c, d, e) -> (a, b, c, d, f)
  fmap f (a, b, c, d, e) = (a, b, c, d, f e)

-- main = print (fmap' (+ 1) (Map.fromList [("a", 1), ("b", 2)]), fmap (+ 1) (1, 2, 3, 4, 5))