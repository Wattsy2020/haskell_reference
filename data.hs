data PeaNum = Succ PeaNum | Zero deriving (Show)

makePeaNum :: Int -> PeaNum
makePeaNum 0 = Zero
makePeaNum x = Succ $ makePeaNum $ x - 1

-- main = print (Zero, Succ Zero, makePeaNum 5)

-- TODO: implement bst insertion, and binary search
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

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

main = do
  let tree = fromList [6, 3, 1, 5, 8, 9]
  print (tree, member 5 tree, member 10 tree)
