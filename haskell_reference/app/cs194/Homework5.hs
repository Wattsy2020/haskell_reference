module Homework5 where

import Data.Maybe
import Data.Function (on)
import Data.Foldable (maximumBy)
import StringUtils

halveEvens :: (Integral a, Fractional a) => [a] -> [a]
halveEvens = mapMaybe (\x -> if even x then Just (x / 2) else Nothing)

holes :: [a] -> [[a]]
holes [] = []
holes (item : remaining) = remaining : map (item :) (holes remaining)

longestText :: Show a => [a] -> a
longestText = maximumBy (compare `on` (length . show))

adjacents :: [a] -> [(a, a)]
adjacents [] = []
adjacents [_] = []
adjacents full@(_: xs) = zip full xs

commas :: [String] -> String
commas [] = ""
commas strs = foldl1 (\result str -> result ++ ", " ++ str) strs

sumTuple :: (Num a) => (a, a) -> a
sumTuple (x, y) = x + y

sumNumbers :: String -> Integer
sumNumbers = sumTuple . foldl fold' (0, 0) . map readDigit
    where
        fold' :: (Integer, Integer) -> Maybe Integer -> (Integer, Integer)
        fold' (total, currentNumber) Nothing = (total + currentNumber, 0)
        fold' (total, currentNumber) (Just digit) = (total, 10*currentNumber + digit)

homework5Main :: IO ()
homework5Main = do
    print $ holes "Hello"
    print $ longestText ([1231, 1, 2, -1, -1231] :: [Integer])
    print $ adjacents ([1, 2, 3, 4, 5] :: [Integer])
    print $ commas ["Hello", "New", "World"]
    print $ sumNumbers "words0are1234separated12by3integers45678"
