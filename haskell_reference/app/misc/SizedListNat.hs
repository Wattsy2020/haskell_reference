{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module SizedListNat where

import GHC.TypeLits

data List (size :: Natural) elem where
  Nil :: List 0 elem
  Cons :: elem -> List n elem -> List (n + 1) elem

instance (Show elem) => Show (List n elem) where
  show :: List n elem -> String
  show Nil = "Nil"
  show (Cons x xs) = show x <> " : " <> show xs

instance Functor (List n) where
  fmap :: (a -> b) -> List n a -> List n b
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

singleton :: elem -> List 1 elem
singleton x = Cons x Nil

headList :: List (n + 1) elem -> elem
headList (Cons x xs) = x

lastList :: List (n + 1) elem -> elem
lastList (Cons x Nil) = x
lastList (Cons x xs@(Cons _ _)) = lastList xs

append :: List n elem -> elem -> List (n + 1) elem
append Nil item = singleton item
append (Cons x xs) item = Cons x $ append xs item

{- this still doesn't work
reverseList' :: List n elem -> List m elem -> List (n + m) elem
reverseList' Nil acc = acc
reverseList' (Cons x xs) acc = reverseList' xs (Cons x acc)

reverseList :: List n elem -> List n elem
reverseList Nil = Nil
reverseList (Cons x xs) = reverseList' xs (Cons x Nil)
-}

-- no idea how to get the length from the type
-- lengthList :: KnownNat n => List n elem -> Integer
-- lengthList = natVal

main :: IO ()
main = do
  print list
  print list2
  print $ headList list
  print $ headList list2
  -- print $ headList Nil -- rejected by compiler
  print $ lastList list
  where
    list = Cons 5 $ Cons 2 $ Cons 3 Nil
    list2 = singleton 10
