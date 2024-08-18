module Homework7 where
import Data.List (genericIndex)
import Control.Applicative (Applicative(liftA2))
import System.Random
import Control.Monad (replicateM)


fibs :: [Integer]
fibs = 1 : 1 : go 1 1
    where
        go :: Integer -> Integer -> [Integer]
        go fib1 fib2 = let fib3 = fib1 + fib2 in
            fib3 : go fib2 fib3

-- imo less clear version
fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

fib :: Integer -> Integer
fib = genericIndex fibs

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show :: Show a => Stream a -> String
  show = show . streamTake (20 :: Int)

streamTake :: Integral i => i -> Stream a -> [a]
streamTake 0 _ = []
streamTake n (Cons x xs) = x : streamTake (n - 1) xs

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamIterate :: (a -> a) -> a -> Stream a
streamIterate f seed = Cons seed (streamIterate f (f seed))

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x s1) s2 = Cons x (streamInterleave s2 s1)

nats :: Stream Integer
nats = streamIterate (+1) 0

ruler :: Stream Integer
ruler = streamInterleave (streamRepeat 0) (streamMap (+1) ruler)

newtype Supply s a = S (Stream s -> (a, Stream s))

get :: Supply s s
get = S (\(Cons x xs) -> (x, xs))

pureSupply :: a -> Supply s a
pureSupply pureResult = S (pureResult,)

mapSupply :: (a -> b) -> Supply s a -> Supply s b
mapSupply f (S supply) = S (\stream -> let (result, remaining) = supply stream in (f result, remaining))

mapSupply2 :: forall a b c s. (a -> b -> c) -> Supply s a -> Supply s b -> Supply s c
mapSupply2 f (S supply1) (S supply2) = S go
    where
        go :: Stream s -> (c, Stream s)
        go stream = let (result1, rem1) = supply1 stream
                        (result2, rem2) = supply2 rem1
                     in (f result1 result2, rem2)

bindSupply :: Supply s a -> (a -> Supply s b) -> Supply s b
bindSupply (S supply) f =
    S (\stream -> let (result, remaining) = supply stream
                      (S newSupply) = f result
                   in newSupply remaining)

runSupply :: Stream s -> Supply s a -> a
runSupply stream (S supply) = fst $ supply stream

instance Functor (Supply s) where
  fmap :: (a -> b) -> Supply s a -> Supply s b
  fmap = mapSupply

instance Applicative (Supply s) where
  pure :: a -> Supply s a
  pure = pureSupply

  liftA2 :: (a -> b -> c) -> Supply s a -> Supply s b -> Supply s c
  liftA2 = mapSupply2

instance Monad (Supply s) where
  (>>=) :: Supply s a -> (a -> Supply s b) -> Supply s b
  (>>=) = bindSupply

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving Show

treeSupply :: Tree a -> Supply s (Tree s)
treeSupply (Leaf _) = Leaf <$> get
treeSupply (Node left' right') = Node <$> treeSupply left' <*> treeSupply right'
-- or liftA2 Node (go left') (go right')

labelTree :: Tree a -> Tree Integer
labelTree = runSupply nats . treeSupply

type Rand a = Supply Integer a

randomDice :: RandomGen g => g -> Stream Integer
randomDice gen =
    let (roll, gen') = randomR (1,6) gen
    in Cons roll (randomDice gen')

runRand :: Rand a -> IO a
runRand r = do
    stdGen <- getStdGen
    let diceRolls = randomDice stdGen
    return $ runSupply diceRolls r

averageOfTwo :: Rand Double
averageOfTwo = do
    d1 <- get
    d2 <- get
    return $ fromIntegral (d1 + d2) / 2

bestOutOfTwo :: Rand Double
bestOutOfTwo = do
    d1 <- get
    d2 <- get
    return $ fromIntegral $ max d1 d2

-- Look, ma, I’m recursive!
sumUntilOne :: Rand Double
sumUntilOne = do
    d <- get
    if d == 1 then return 0
                else do s <- sumUntilOne
                        return (s + fromIntegral d)

sample :: Int -> Rand Double -> Rand (Double, Double)
sample n what = do
    samples <- replicateM n what
    return (maximum samples, sum samples / fromIntegral n)

homework7Main :: IO ()
homework7Main = mapM_ go [ ("average of two", averageOfTwo)
                , ("bestOutOfTwo",   bestOutOfTwo)
                , ("sumUntilOne",    sumUntilOne)
                ]
  where
    n = 10000
    go (name, what) = do
        (maxresult, avg) <- runRand (sample n what)
        putStrLn $ "Playing \"" ++ name ++ "\" " ++ show n ++ " times " ++
                   "yields a max of " ++ show maxresult ++ " and an average of " ++
                   show avg ++ "."
