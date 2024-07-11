{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

data Natural = Zero | Succ Natural
type One = Succ Zero

type family Add (num1 :: Natural) (num2 :: Natural) where
    Add x1 Zero = x1
    Add Zero x2 = x2
    Add (Succ x1) x2 = Succ (Add x1 x2)

data List (size :: Natural) elem where
    Nil :: List 'Zero elem
    Cons :: elem -> List n elem -> List ('Succ n) elem

instance (Show elem) => Show (List n elem) where
  show :: List n elem -> String
  show Nil = "Nil"
  show (Cons x xs) = show x <> " : " <> show xs

instance Functor (List n) where
  fmap :: (a -> b) -> List n a -> List n b
  fmap = mapList

singleton :: elem -> List One elem
singleton x = Cons x Nil

headList :: List (Succ n) elem -> elem
headList (Cons x xs) = x

lastList :: List (Succ n) elem -> elem
lastList (Cons x Nil) = x
lastList (Cons x xs@(Cons _ _)) = lastList xs

append :: List n elem -> elem -> List (Add n One) elem
append Nil item = singleton item
append (Cons x xs) item = Cons x $ append xs item

{-
-- encoding that (Cons x xs) gives a list xs with length N - 1 is tricky
-- also this isn't recognising that n + m++ == n++ + m
-- we might instead need to use KnownNat from 
-- https://hackage.haskell.org/package/base-4.20.0.1/docs/GHC-TypeLits.html 
reverseList' :: List n elem -> List m elem -> List (Add n m) elem
reverseList' Nil acc = acc
reverseList' (Cons x xs) acc = reverseList' xs (Cons x acc)

reverseList :: List n elem -> List n elem
reverseList Nil = Nil
reverseList (Cons x xs) = reverseList' xs (Cons x Nil)
-}

foldList :: (acc -> elem -> acc) -> acc -> List n elem -> acc
foldList _ acc Nil = acc
foldList f acc (Cons x xs) = foldList f (f acc x) xs

safeFold1 :: (elem -> elem -> elem) -> List (Succ n) elem -> elem
safeFold1 f (Cons x xs) = foldList f x xs

foldrList :: (elem -> acc -> acc) -> acc -> List n elem -> acc
foldrList _ acc Nil = acc
foldrList f acc (Cons x xs) = f x (foldrList f acc xs)

mapList :: (a -> b) -> List n a -> List n b
mapList _ Nil = Nil
mapList f (Cons x xs) = Cons (f x) (mapList f xs)

-- todo: see if we can get the length in constant time using KnowNat
lengthList :: List n b -> Int
lengthList = foldList (\acc x -> acc + 1) 0

{-
Also tricky: doesn't recognise commutation
concatList :: List n elem -> List m elem -> List (Add n m) elem
concatList Nil xs = xs
concatList xs Nil = xs
concatList (Cons x xs) second = Cons x (concatList xs second)
-}

unconsList :: List (Succ n) elem -> (elem, List n elem)
unconsList (Cons x xs) = (x, xs)

{- type constraint issues as usual
unsnocList' :: List n elem -> List m elem -> elem -> (List (Add n m) elem, elem)
unsnocList' Nil acc x = (acc, x)
unsnocList' (Cons x xs) acc prevx = unsnocList' xs (Cons prevx acc) x

unsnocList :: List (Succ n) elem -> (List n elem, elem)
unsnocList (Cons x Nil) = (Nil, x)
unsnocList (Cons x xs) = unsnocList' xs Nil x
-}

-- todo: implement splitAt N, which splits a list into two lists
-- one with the first N items and second with the remaining items

{- This also can't be implemented, since we don't know the size of the result list
   We need something like "AnyNat"
filterList :: (elem -> Bool) -> List n elem -> List m elem
filterList _ Nil = Nil
filterList f (Cons x xs)
  | f x = Cons x $ filterList f xs
  | otherwise = filterList f xs
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
  print $ mapList (+ 2) list
  print $ fmap (*2) list
  print $ foldList (*) 1 list
  print $ safeFold1 (*) list
  print $ lengthList list
  print $ unconsList list
  print $ unconsList list2
  where
    list = Cons 5 $ Cons 2 $ Cons 3 Nil
    list2 = singleton 10
