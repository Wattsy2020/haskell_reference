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
dropWhile' predicate (x : xs)
  | predicate x = dropWhile' predicate xs
  | otherwise = x : xs

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

main = print (length $ takeRange 0 100 primes, length $ takeRange 100 1000 primes, length $ takeRange 900 1000 primes)