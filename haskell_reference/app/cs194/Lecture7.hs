module Lecture7 where

-- this is bad because it's not tail recursive
-- every element in the list generates a new function call, taking up memory
badSum :: Num a => [a] -> a
badSum [] = 0
badSum (x:xs) = x + badSum xs

-- this is better, but because of lazy evaluation it generates a thunk for each + operation
-- again taking up extra memory
tailRecursiveSum :: Num a => [a] -> a
tailRecursiveSum = go 0
    where
        go :: Num a => a -> [a] -> a
        go acc [] = acc
        go acc (x:xs) = go (x + acc) xs

-- this is best, it strictly evaluates the sum, so no thunks are stored
-- `seq` strictly evaluates its first argument, then returns the second argument
strictSum :: Num a => [a] -> a
strictSum = go 0
    where
        go :: Num a => a -> [a] -> a
        go acc [] = acc
        go acc (x:xs) = acc `seq` go (x + acc) xs
