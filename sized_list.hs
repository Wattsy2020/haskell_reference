{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

data Natural = Zero | Succ Natural
type One = Succ Zero

type family Add (num1 :: Natural) (num2 :: Natural) where
    Add 'Zero x = x
    Add ('Succ x) x2 = 'Succ (Add x x2)

data List (size :: Natural) elem where
    Nil :: List 'Zero elem
    Cons :: elem -> List n elem -> List ('Succ n) elem

instance (Show elem) => Show (List n elem) where
  show :: List n elem -> String
  show Nil = "Nil"
  show (Cons x xs) = show x <> " : " <> show xs

singleton :: elem -> List One elem
singleton x = Cons x Nil

headList :: List (Succ n) elem -> elem
headList (Cons x xs) = x

lastList :: List (Succ n) elem -> elem
lastList (Cons x Nil) = x
lastList (Cons x xs@(Cons _ _)) = lastList xs

append :: List n elem -> elem -> List (Add n One) elem
append Nil item = Cons item Nil
append (Cons x xs) item = Cons x $ append xs item

{-
reverseList' :: List a b -> List a NonEmpty -> List a NonEmpty
reverseList' Nil acc = acc
reverseList' (Cons x xs) acc = reverseList' xs (Cons x acc)

-- can also preserve the type of b
reverseList :: List a b -> List a b
reverseList Nil = Nil
reverseList (Cons x xs) = reverseList' xs (Cons x Nil)

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
concatList Nil Nil = Nil
concatList Nil xs@(Cons _ _) = xs
concatList xs@(Cons _ _) Nil = xs
concatList (Cons x xs) second@(Cons _ _) = Cons x (concatList xs second)

-- this one also give errors when trying to implement
-- because b could be either Empty or NonEmpty
-- and we don't have enough information at the type level to determine which it is
-- to determine whether its empty or not, we need to encode the full length at the type level
-- unconsList :: List a NonEmpty -> (a, List a b)
-- unconsList (Cons x Nil) = (x, Nil)
-- unconsList (Cons x (Cons _ _)) = (x, Cons x Nil)

-- todo: implement splitAt N, which splits a list into two lists
-- one with the first N items and second with the remaining items
-}

main :: IO ()
main = do
  print list
  print list2
  print $ headList list
  print $ headList list2
  -- print $ headList Nil -- rejected by compiler
  print $ lastList list
  print $ append list 1
  {-
  print $ reverseList list
  print $ mapList (+ 2) list
  print $ foldList (*) 1 list
  print $ safeFold1 (*) list
  --  print $ concatList list list
  print $ lengthList list -}
  where
    list = Cons 5 $ Cons 2 $ Cons 3 Nil
    list2 = singleton 10
