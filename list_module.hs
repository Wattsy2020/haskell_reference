-- import Data.List
-- reimplementations of functions in Data.List

-- add x to the list if it is not already contained in it
addIfUnique :: Eq a => a -> [a] -> [a]
addIfUnique x xs
  | x `elem` xs = xs
  | otherwise = x : xs

-- return a list with only unique elements
nub' :: Eq a => [a] -> [a]
nub' = foldr addIfUnique []

-- main = print (nub' [1, 2, 3, 4, 3, 2], nub' [1, 1, 1], nub' [1, 2, 3])

-- intersperse takes an element and a list and then puts that element in between each pair of elements in the list
intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' separator xs = head xs : inserted
  where
    inserted = foldr (\x acc -> separator : x : acc) [] (tail xs)

-- main = print (intersperse' '.' "Hello", intersperse' '.' "", intersperse' [0, 0, 0] [[1, 1, 1], [2, 2, 2]])

-- concat: flattens a list of lists into a list of their elements
concat' :: [[a]] -> [a]
concat' = foldl (++) []

-- main = print (concat' ["Hello", " ", "World"], concat' [[1, 2], [3, 4], [5, 6]])

-- intercalate takes a list of lists and a list. It then inserts that list in between all those lists and then flattens the result.
intercalate' :: [a] -> [[a]] -> [a]
intercalate' separator = concat' . intersperse' separator

-- main = print $ intercalate' " " ["hey", "there", "guys"]

-- Doing concatMap is the same as first mapping a function to a list and then concatenating the list with concat.
concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = concat' . map f

-- main = print $ concatMap (replicate 4) [1 .. 3]

and' :: [Bool] -> Bool
and' = foldl (&&) True

or' :: [Bool] -> Bool
or' = foldl (||) False

-- main =
--  print (map and' items, map or' items)
--  where
--    items = [[False, False, True], [True, True, True]]

-- any and all take a predicate and then check if any or all the elements in a list satisfy the predicate, respectively
all' :: (a -> Bool) -> [a] -> Bool
all' f = or' . map f

any' :: (a -> Bool) -> [a] -> Bool
any' f = and' . map f

main = print (any' (== 4) [2, 3, 5, 6, 1, 4], all (> 4) [6, 9, 10], all (`elem` ['A' .. 'Z']) "Yo", any (`elem` ['A' .. 'Z']) "Yo")
