module Reference where

nums :: [Int] = [1, 2, 3, 4]

squared = map (^ 2) nums

factorial :: Int -> Int
factorial n
  | n == 0 = 1
  | otherwise = n * factorial (n - 1)

-- print (map factorial nums)
-- main = print(map ((^ 2) . factorial) nums) -- can compose functions with .

fib :: Int -> Int
fib n
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)

-- main = print (fib 10)

-- there is a filter function
-- main = print (filter even nums)

-- patern matching example
-- also typeclasses: Ord a represents any type that implements an Ordering checker
maximum' :: (Ord a) => [a] -> a
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)

-- main = print (maximum' nums, maximum' "Hello", maximum' [(1, "hello"), (1, "hezzo")])
-- >(4,'o',(1,"hezzo"))

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

-- main = print (sum' nums, sum' [1.2, 3.4, 5.6], sum' [])
-- >(10, 10.2, 0)

-- composing functions
addOne :: (Integral a) => [a] -> [a]
addOne = map (+ 1)

filterFunc :: (Integral a) => [a] -> [a]
filterFunc = filter even

-- (func1 . func2) creates a new function, need to wrap in brackets to apply it
result = (addOne . filterFunc) nums

-- main = print result

-- given a function, return a function that applies that function twice
twice :: (a -> a) -> (a -> a)
twice func = func . func

addFour = (twice . twice) addOne

main = print (addFour nums)
