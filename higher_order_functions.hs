import Data.Foldable (minimumBy)
import Data.Function (on)
import Distribution.Simple.Setup (ConfigFlags (configAllowDependingOnPrivateLibs))
import Text.XHtml (base)

all' :: [Bool] -> Bool
all' [] = True -- vaccuous truth
all' [x] = x
all' (x : xs) = x && all' xs

-- main = print (all' [True, True], all' [True, True, False])

replicate' :: Integral b => a -> b -> [a]
replicate' item times
  | times < 0 = error "cannot replicate a negative amount of times"
  | times == 0 = []
  | otherwise = item : replicate' item (times - 1)

-- main = print (replicate' "hello" 4)

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

-- main = print (zip' [1, 2, 3] [1, 2])

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' item (x : xs) = item == x || elem' item xs

-- main = print (elem' 1 [3, 1, 2], elem' "Hello" ["There", "Kenobi", "Hello"], elem' 42 [1, 2, 3])

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

--  main = print (zipWith' (+) [1,2,3] [4, 5, 6])

dotProd :: Num a => [a] -> [a] -> a
dotProd xs ys = sum (zipWith (*) xs ys)

-- main = print (dotProd [1, 2, 3] [4, 5, 6])

largest = head (filter (\x -> x `mod` 3829 == 0) [100000, 99999 .. 1])

-- main = print largest

squareSum = sum (filter odd (takeWhile (< 10000) [x ^ 2 | x <- [1 ..]]))

-- main = print squareSum

nextCollatz :: Integral a => a -> a
nextCollatz x
  | odd x = 3 * x + 1
  | otherwise = x `div` 2

-- create collatzChain using a generic function that recursively calls a function to construct a list
-- function takes f, an initial value, then recursively calls f to create the list
recurseList :: (a -> a) -> a -> [a]
recurseList f x = x : recurseList f (f x)

collatzChain :: Integral a => a -> [a]
collatzChain = recurseList nextCollatz

-- main = print (take 10 (collatzChain 9))

-- take while the function is True, including the first false element
takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 _ [] = []
takeWhile1 f (x : xs)
  | f x = x : takeWhile1 f xs
  | otherwise = [x]

-- main = print (takeWhile1 (1 /=) (collatzChain 5))

collatzLength :: Int -> Int
collatzLength x = length (takeWhile (1 /=) (collatzChain x))

numLongCollatz = length (filter (> 15) (map collatzLength [1 .. 100]))

-- main = print numLongCollatz

-- compute the moving average of a list
expMean :: Fractional a => [a] -> [a]
expMean = scanl1 (\acc x -> 0.9 * acc + 0.1 * x)

-- main = print (expMean (map (fromIntegral . collatzLength) [1..100]))

-- Implement with folding
all2 :: [Bool] -> Bool
all2 = foldl (&&) True -- can also just use the and func
-- main = print (all2 [True, True], all2 [True, True, False])

elem2 :: Eq a => a -> [a] -> Bool
elem2 item = foldl (\acc x -> acc || item == x) False

-- main = print (elem2 1 [3, 1, 2], elem2 "Hello" ["There", "Kenobi", "Hello"], elem2 42 [1, 2, 3], elem2 1 [])

sum' :: Num a => [a] -> a
sum' = foldr (+) 0

-- main = print (sum [1, 2, 3, 4])

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- main = print (map' (+1) [1, 2, 3, 4])

-- scan function, applies a cumulative function to each element in the list, outputing the results as a list
-- where list !! i is the ith cumulative result
scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' _ _ [] = []
scanl' f acc (x : xs) = new_acc : scanl' f new_acc xs
  where
    new_acc = f acc x

-- scanl1 just uses the first element in the list as the starting accumlulator arg
scanl1' :: (a -> a -> a) -> [a] -> [a]
scanl1' f (x : xs) = scanl' f x xs

-- can use the $ to lower the priority and evaluate the right arguments first
main = print $ scanl1' (+) [1 .. 10]

-- how many square roots of the natural numbers does it take to get for their sum to be over 1000
cumulativeSumSqrts = scanl1 (+) (map sqrt [1 ..])

answer = length (takeWhile (< 1000) cumulativeSumSqrts)

-- main = print answer

-- how foldl is defined
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' accFunc acc (x : xs) = foldl' accFunc (accFunc acc x) xs

-- define elem using currying
elem3 :: Eq a => a -> [a] -> Bool
elem3 item = foldl' (\acc x -> acc || (item == x)) False

-- main = print (elem3 1 [3, 1, 2], elem3 "Hello" ["There", "Kenobi", "Hello"], elem3 'H' "Hello", elem3 42 [1, 2, 3], elem3 1 [])

distance :: Integral a => a -> a -> a
distance a b = abs (a - b)

-- a = (compare `on` distance 1) 0 2 -- the `on` function means: call (distance 1) on the two elements first, then apply compare to the two resulting values
on' :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on' pairFunc elemFunc input1 input2 = pairFunc (elemFunc input1) (elemFunc input2)

-- main = print (on' (+) abs (-1) (-2)) -- abs(-1) + abs(-2) = 3

closestElem :: Int -> [Int] -> Int
closestElem target = minimumBy (compare `on'` distance target)

-- main = print (closestElem 10 [1, 2, 3, 8, 11, 15])

-- main == print ((compare `on` distance 1) [1, 2])