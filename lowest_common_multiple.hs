import Data.Foldable (toList)
import Data.Sequence qualified as Seq

-- find the smallest number that is divisible by all numbers from 1 to 10 (i.e. the LCM)
-- can do this by finding the prime factorization of each number, then taking element wise max across all the factors
-- e.g. 2 has factorization 2^1, 8 has 2^3, take element wise max of 2 to find out we only need 2^3 in our final LCM

-- 1. Construct the primes
isPrime :: Seq.Seq Int -> Int -> Bool
isPrime primes x = not $ any (\num -> mod x num == 0) filteredPrimes
  where
    -- primes > 1 and less than sqrt x
    filteredPrimes = Seq.takeWhileL (<= (floor $ sqrt $ fromIntegral x)) (Seq.drop 1 primes)

-- main = print $ map (isPrime $ Seq.fromList [1, 2, 3, 5, 7]) [8, 9, 10, 11]

nextPrime :: Seq.Seq Int -> Int
nextPrime Seq.Empty = 1
nextPrime primes = head $ filter (isPrime primes) [last_prime + 1 ..]
  where
    _ Seq.:|> last_prime = primes
    next_nums = [last_prime + 1 ..]

-- main = print $ nextPrime $ Seq.fromList [1, 2, 3, 5, 7]

-- like iterate, but allows for passing context between the functions
iterateContext :: (b -> a) -> (b -> a -> b) -> b -> Seq.Seq a
iterateContext f accFunc context = next Seq.<| iterateContext f accFunc (accFunc context next)
  where
    next = f context

-- get first n primes
primes :: Int -> Seq.Seq Int
primes n = foldl addNextPrime Seq.Empty [1 .. n]
  where
    addNextPrime acc _ = acc Seq.|> nextPrime acc

primes' = toList (primes 10)

-- main = print $ primes 10

-- main = print (nextPrime Seq.Empty, nextPrime $ Seq.fromList [1], nextPrime $ Seq.fromList [1, 2, 3])

timesDivisible :: Int -> Int -> Int
timesDivisible _ 1 = 0 -- just to prevent infinite recurision
timesDivisible num divisor
  | num `mod` divisor == 0 = 1 + timesDivisible (num `div` divisor) divisor
  | otherwise = 0

-- main = print (timesDivisible 8 2, timesDivisible 15 3, timesDivisible 9 5)

-- get the prime factorization of a number as a list x
-- where x !! i is the power of the ith prime in the factorization
factorize :: Int -> [Int]
factorize n = map (timesDivisible n) primes'

-- convert a factored list back to an integer
factoredToInt :: [Int] -> Int
factoredToInt factorCount = product $ filter (/= 0) factors
  where
    factors = zipWith (^) primes' factorCount

-- main = print (map factorize [1 .. 10], map (factoredToInt . factorize) [1 .. 10])

elementMax :: [Int] -> [Int] -> [Int]
elementMax = zipWith max

-- find the LCM of all numbers up to n
lcm' :: [Int] -> Int
lcm' numbers = factoredToInt commonFactors
  where
    factorized = map factorize numbers
    commonFactors = foldl1 elementMax factorized

-- easier method using haskell's lcm
lcm2 :: [Int] -> Int
lcm2 = foldl lcm 1

main = print (lcm' [1 .. 10], lcm2 [1 .. 10])
