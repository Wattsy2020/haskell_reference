data PeaNum = Succ PeaNum | Zero deriving (Show)

makePeaNum :: Int -> PeaNum
makePeaNum 0 = Zero
makePeaNum x = Succ $ makePeaNum $ x - 1

-- main = print (Zero, Succ Zero, makePeaNum 5)

data Complex a = Complex a a deriving (Show)

fromReal :: Num a => a -> Complex a
fromReal x = Complex x 0

add :: Num a => Complex a -> Complex a -> Complex a
add (Complex x1 y1) (Complex x2 y2) = Complex (x1 + x2) (y1 + y2)

multiply :: Num a => Complex a -> Complex a -> Complex a
multiply (Complex x1 y1) (Complex x2 y2) = Complex (x1 * x2 - y1 * y2) (x1 * y2 + x2 * y1)

pow :: Num a => Complex a -> Int -> Complex a
pow c1 times = foldl (\acc x -> multiply c1 acc) c1 [1 .. times]

main = do
  let c1 = Complex 1 1
  let c2 = Complex 2 2.5
  let i = Complex 0 1
  print (add c1 c2, multiply c1 c2, multiply c1 i, pow i 2, pow i 4)
  print (fromReal 2 `multiply` c1)

-- TODO: implement bst insertion, and binary search
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

-- TODO: make it a self balancing Tree
insert :: Ord a => a -> Tree a -> Tree a
insert item Leaf = Node Leaf item Leaf
insert item (Node leftTree nodeVal rightTree)
  | item < nodeVal = Node (insert item leftTree) nodeVal rightTree
  | otherwise = Node leftTree nodeVal (insert item rightTree)

fromList :: Ord a => [a] -> Tree a
fromList = foldl (flip insert) Leaf

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

{-
main = do
  let tree = fromList [6, 3, 1, 5, 8, 9]
  print (tree, member 5 tree, member 10 tree, inOrder tree, height tree, treeLength tree)
-}
