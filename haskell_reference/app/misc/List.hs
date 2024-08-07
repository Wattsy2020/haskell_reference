module List where

import Data.Char (isSpace)
import Data.Function (on)

-- reimplementations of functions in Data.List

-- add x to the list if it is not already contained in it
addIfUnique :: (Eq a) => a -> [a] -> [a]
addIfUnique x xs
  | x `elem` xs = xs
  | otherwise = x : xs

-- return a list with only unique elements
nub' :: (Eq a) => [a] -> [a]
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

-- main = print (any' (== 4) [2, 3, 5, 6, 1, 4], all (> 4) [6, 9, 10], all (`elem` ['A' .. 'Z']) "Yo", any (`elem` ['A' .. 'Z']) "Yo")

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' idx xs = (take idx xs, drop idx xs)

-- main = print $ splitAt' 5 "hello there"

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' predicate (x : xs)
  | predicate x = x : takeWhile' predicate xs
  | otherwise = []

-- note we cannot use foldl, since takeWhile has early stopping, using foldl (actually foldr) gives us filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' predicate = foldr (\x acc -> if predicate x then x : acc else acc) []

-- main = do
--  print (takeWhile' (> 3) [6, 5, 4, 3, 2, 1, 2, 3, 4], takeWhile' (/= ' ') "This is a sentence")
--  print (filter' (> 3) [6, 5, 4, 3, 2, 1, 2, 3, 4], filter' (/= ' ') "This is a sentence")

-- main = print $ sum $ takeWhile' (< 10000) (map (^ 3) [1 ..])

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' predicate xs@(x : xs')
  | predicate x = dropWhile' predicate xs'
  | otherwise = xs

-- main = print (dropWhile (/= ' ') "This is a sentence", dropWhile (< 3) [1, 2, 2, 2, 3, 4, 5, 4, 3, 2, 1])

showStock :: (Float, Int, Int, Int) -> String
showStock (price, year, month, day) = "price=" ++ show price ++ " date=" ++ show year ++ "/" ++ show month ++ "/" ++ show day

-- main = do
--  let stock = [(994.4, 2008, 9, 1), (995.2, 2008, 9, 2), (999.2, 2008, 9, 3), (1001.4, 2008, 9, 4), (998.3, 2008, 9, 5)]
--  print $ map showStock (dropWhile' (\(price, _, _, _) -> price < 1000) stock)

-- build prime sieve of erasmus and calculate number of primes between 100 and 1000, compare to 0 and 100
-- given a list of primes less than a number, and the number, calculate whether it is a prime
isPrime :: [Int] -> Int -> Bool
isPrime primes x = not $ any (\num -> mod x num == 0) filteredPrimes
  where
    -- primes > 1 and less than sqrt x
    filteredPrimes = takeWhile' (<= (floor $ sqrt $ fromIntegral x)) (tail primes)

nextPrime :: [Int] -> Int
nextPrime [] = 1
nextPrime primes = head $ filter (isPrime primes) [last primes + 1 ..]

-- main = print (map (isPrime [1, 2, 3, 5, 7]) [8, 9, 10, 11], nextPrime [1, 2, 3, 5, 7])

-- like iterate, but allows for passing context as a list between the functions
iterateContext :: (b -> a) -> (b -> a -> b) -> b -> [a]
iterateContext f accFunc context = next : iterateContext f accFunc (accFunc context next)
  where
    next = f context

primes :: [Int]
primes = iterateContext nextPrime (\acc x -> acc ++ [x]) []

takeRange :: Int -> Int -> [Int] -> [Int]
takeRange low high = takeWhile' (< high) . dropWhile' (< low)

-- main = print (length $ takeRange 0 100 primes, length $ takeRange 100 1000 primes, length $ takeRange 900 1000 primes)

-- iterateContext can be used to define the fibonacci sequence in a "backwards recursive" manner, no need for memoization!
-- in reality this is just like an infinite generator in python
fib :: [Integer]
fib = iterateContext (uncurry (+)) (\(_, n1) n -> (n1, n)) (0, 1)

-- main = print $ sum $ take 10000 fib
phi :: Double
phi = (1 / 2) * (1 + sqrt 5)

-- can see that fib n / fib n-1 approaches phi
-- main = do
--  let fibRatio = zipWith ((/) `on` fromInteger) (tail fib) fib
--  print $ take 20 $ map (/ phi) fibRatio

span' :: (a -> Bool) -> [a] -> ([a], [a])
span' _ [] = ([], [])
span' predicate xs@(x : xs')
  | predicate x =
      let (included, remaining) = span' predicate xs'
       in (x : included, remaining)
  | otherwise = ([], xs)

break' :: (a -> Bool) -> [a] -> ([a], [a])
break' predicate = span' (not . predicate)

addToPartition :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
addToPartition predicate x (matching, other)
  | predicate x = (x : matching, other)
  | otherwise = (matching, x : other)

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' predicate = foldr (addToPartition predicate) ([], [])

testList2 = [1, 2, 3, 4, 5, 6, 7, 3, 9]

-- main = print (span' (< 5) testList2, break (== 5) testList2, partition' (< 5) testList2)

-- add newElem to the current list if it is the same elem, otherwise create a new list
accAdjacent :: (Eq a) => a -> [[a]] -> [[a]]
accAdjacent newElem [[]] = [[newElem]]
accAdjacent newElem accLists@(accList : accLists')
  | newElem == head accList = (newElem : accList) : accLists'
  | otherwise = [newElem] : accLists

group' :: (Eq a) => [a] -> [[a]]
group' = foldr accAdjacent [[]]

-- main = print $ group' [1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 2, 2, 2, 5, 6, 7]

head' :: [a] -> a
head' (x : _) = x

last' :: [a] -> a
last' [x] = x
last' (_ : xs) = last' xs

init' :: [a] -> [a]
init' [_] = []
init' (x : xs) = x : init' xs

tail' :: [a] -> [a]
tail' (_ : xs) = xs

testList :: [Int]
testList = [1, 2, 3, 4]

{-
main =
  print $
    "head: "
      ++ show (head' testList)
      ++ " last: "
      ++ show (last' testList)
      ++ " init: "
      ++ show (init' testList)
      ++ " tail: "
      ++ show (tail' testList)
-}

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

-- main = print (reverse' [1, 2, 3, 4, 5], reverse' "hello there!")

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' xs = xs : tails' (tail' xs)

-- this might seem like a hack, but we need to use scanr on a dummy list of the same length, just so we know:
-- "after going through the whole list, return the list and then start shrinking that list with init"
inits' :: [a] -> [[a]]
inits' xs = scanr (\_ acc -> init' acc) xs xs

-- alternate implementation has expensive list concatenation
-- inits' [] = [[]]
-- inits' xs = inits' (init xs) ++ [xs]

-- main = print (tails' [1, 2, 3, 4], inits' [1, 2, 3, 4])

isPrefixOf' :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf' prefix xs = prefix == take (length prefix) xs

isSuffixOf' :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf' suffix = isPrefixOf' (reverse' suffix) . reverse'

-- main = print (isPrefixOf' "hello" "hello there!", isPrefixOf' "hello" "hey", isSuffixOf' "there" "hello there")

find' :: (a -> Bool) -> [a] -> Maybe a
find' _ [] = Nothing
find' predicate (x : xs)
  | predicate x = Just x
  | otherwise = find' predicate xs

findIndexCount :: Int -> (a -> Bool) -> [a] -> Maybe Int
findIndexCount _ _ [] = Nothing
findIndexCount idx predicate (x : xs)
  | predicate x = Just idx
  | otherwise = findIndexCount (idx + 1) predicate xs

findIndex' = findIndexCount 0

testList3 = [1, 2, 3, 4, 5, 6]

-- main = print (find' (> 4) testList3, find' (> 9) testList3, findIndex' (> 4) testList3)

elemIndexCount :: (Eq a) => Int -> a -> [a] -> Maybe Int
elemIndexCount _ _ [] = Nothing
elemIndexCount idx item (x : xs)
  | item == x = Just idx
  | otherwise = elemIndexCount (idx + 1) item xs

elemIndex' :: (Eq a) => a -> [a] -> Maybe Int
elemIndex' = elemIndexCount 0

-- main = print (elemIndex' 4 testList3, elemIndex' 1 testList3, elemIndex' 10 testList3)

elemIndicesCount :: (Eq a) => Int -> a -> [a] -> [Int]
elemIndicesCount _ _ [] = []
elemIndicesCount idx item (x : xs)
  | item == x = idx : remaining
  | otherwise = remaining
  where
    remaining = elemIndicesCount (idx + 1) item xs

elemIndices' :: (Eq a) => a -> [a] -> [Int]
elemIndices' = elemIndicesCount 0

-- main = print $ ' ' `elemIndices'` "Where are the spaces?"

-- function to split the string by a given character
splitStrAcc :: Char -> Char -> [String] -> [String]
splitStrAcc splitChar c currLines@(line : remaining)
  | splitChar == c = "" : currLines
  | otherwise = (c : line) : remaining

splitStr :: Char -> String -> [String]
splitStr splitChar = foldr (splitStrAcc splitChar) [""]

lines' :: String -> [String]
lines' = splitStr '\n'

-- main = print (lines "first line\nsecond line\nthird line", splitStr ' ' "Hello there!")

unlines' :: [String] -> String
unlines' = intercalate' "\n"

-- main = print $ unlines ["first line", "second line", "third line"]

-- words removes all whitespace from a string
accWords :: Char -> [String] -> [String]
accWords c words'@(first : remaining)
  | isSpace c =
      case first of
        "" -> words' -- don't add another string if we encounter multiple whitespace chars
        _ -> "" : words'
  | otherwise = (c : first) : remaining

words' :: String -> [String]
words' = foldr accWords [""]

-- main = print (words "hello there", words "hey these           are    the words in this\nsentence")

delete' :: (Eq a) => a -> [a] -> [a]
delete' _ [] = []
delete' item (x : xs)
  | x == item = xs
  | otherwise = x : delete' item xs

-- main = print (delete' 5 [1, 2, 5, 3], delete' 5 [1, 2, 6])

-- remove elements in list1 that are present in list2
difference :: (Eq a) => [a] -> [a] -> [a]
difference = foldl (flip delete')

union' :: (Eq a) => [a] -> [a] -> [a]
union' list1 list2 = list1 ++ difference list2 list1

addIfElem :: (Eq a) => [a] -> a -> [a] -> [a]
addIfElem checkList x currList
  | x `elem` checkList = x : currList
  | otherwise = currList

intersect' :: (Eq a) => [a] -> [a] -> [a]
intersect' list1 list2 = foldr (addIfElem list2) [] list1

-- main = print (difference [1 .. 7] [5 .. 10], union' [1 .. 7] [5 .. 10], intersect' [1 .. 7] [5 .. 10])

-- insert element into a list, preserving the list if it is sorted
insert' :: (Ord a) => a -> [a] -> [a]
insert' item xs = first ++ [item] ++ remaining
  where
    (first, remaining) = span' (<= item) xs

-- main = print (insert' 4 [1, 2, 3, 5, 6, 7], insert' 'g' $ ['a' .. 'f'] ++ ['h' .. 'z'])
