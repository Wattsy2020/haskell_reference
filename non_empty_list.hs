{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- see https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html
-- to understand how functions on types work

-- Emptiness is now a Kind, defining two types that belong to the Emptiness Kind: Empty and NonEmpty
data Emptiness = Empty | NonEmpty

-- type function: a function on all types with Kind of Emptiness, that maps to a type of Kind Emptiness
-- intended for combining Empty and NonEmpty lists, which results in a non empty List
type family Combine (emptyStatus1 :: Emptiness) (emptyStatus2 :: Emptiness) where
  Combine maybeEmpty 'Empty = maybeEmpty
  Combine maybeEmpty 'NonEmpty = 'NonEmpty
  Combine 'Empty maybeEmpty = maybeEmpty
  Combine 'NonEmpty maybeEmpty = 'NonEmpty

-- the second type y is either Empty or NonEmpty, we can use this to control what types a function accepts
data List x (emptiness :: Emptiness) where
  Nil :: List a 'Empty -- here we use 'Empty as a type
  Cons :: a -> List a b -> List a 'NonEmpty

instance (Show a) => Show (List a b) where
  show :: List a b -> String
  show Nil = "Nil"
  show (Cons x xs) = show x <> " : " <> show xs

singleton :: a -> List a NonEmpty
singleton x = Cons x Nil

safeHead :: List a NonEmpty -> a
safeHead (Cons x xs) = x

safeLast :: List a NonEmpty -> a
safeLast (Cons x Nil) = x
safeLast (Cons x xs@(Cons _ _)) = safeLast xs

-- safeHead Nil = error "haskell detects that this is never reached"
-- since Nil constructor always gives a List a Empty

-- Now though we can't define a function that returns either Nil or Cons, the compiler rejects it
-- silly :: Int -> List Int b
-- silly 0 = Nil
-- silly 1 = Cons 1 Nil

-- can however take both and produce a single one
append :: List a b -> a -> List a NonEmpty
append Nil item = Cons item Nil
append (Cons x xs) item = Cons x $ append xs item

reverseList' :: List a b -> List a c -> List a (Combine b c)
reverseList' Nil acc = acc
reverseList' (Cons x xs) acc = reverseList' xs (Cons x acc)

-- can also preserve the type of b
reverseList :: List a b -> List a b
reverseList xs = reverseList' xs Nil 

-- reverseList (Cons x xs) = reverseList xs
-- is detected as an error as reverseList xs is not guaranteed to be Cons

-- derive functor for list
foldList :: (acc -> elem -> acc) -> acc -> List elem emptyNess -> acc
foldList _ acc Nil = acc
foldList f acc (Cons x xs) = foldList f (f acc x) xs

foldrList :: (elem -> acc -> acc) -> acc -> List elem emptyNess -> acc
foldrList _ acc Nil = acc
foldrList f acc (Cons x xs) = f x (foldrList f acc xs)

safeFold1 :: (a -> a -> a) -> List a NonEmpty -> a
safeFold1 f (Cons x xs) = foldList f x xs

mapList :: (a -> c) -> List a b -> List c b
mapList _ Nil = Nil
mapList f (Cons x xs) = Cons (f x) (mapList f xs)

lengthList :: List a b -> Int
lengthList = foldList (\acc x -> acc + 1) 0

-- Concating is difficult
-- We use the Combine type function to prove that combining empty with NonEmpty list is NonEmpty
concatList :: List a b -> List a c -> List a (Combine b c)
concatList Nil xs = xs
concatList xs Nil = xs
concatList (Cons x xs) second = Cons x (concatList xs second)

-- this one also give errors when trying to implement
-- because b could be either Empty or NonEmpty
-- and we don't have enough information at the type level to determine which it is
-- to determine whether its empty or not, we need to encode the full length at the type level
-- unconsList :: List a NonEmpty -> (a, List a b)
-- unconsList (Cons x Nil) = (x, Nil)
-- unconsList (Cons x (Cons _ _)) = (x, Cons x Nil)

main :: IO ()
main = do
  print $ safeHead list
  print $ safeHead list2
  print $ safeLast list
  print $ append list 1
  print $ reverseList list
  print $ mapList (+ 2) list
  print $ foldList (*) 1 list
  print $ safeFold1 (*) list
  print $ concatList list $ reverseList list
  print $ lengthList list
  where
    list = Cons 5 $ Cons 2 $ Cons 3 Nil
    list2 = singleton 10
