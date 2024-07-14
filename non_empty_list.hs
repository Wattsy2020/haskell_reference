{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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

-- the first type is either Empty or NonEmpty, we can use this to control what types a function accepts
data List (emptiness :: Emptiness) elem where
  Nil :: List 'Empty elem -- here we use 'Empty as a type
  Cons :: elem -> List e elem -> List 'NonEmpty elem

instance (Show elem) => Show (List e elem) where
  show :: List e elem -> String
  show Nil = "Nil"
  show (Cons x xs) = show x <> " : " <> show xs

instance Functor (List e) where
  fmap :: (elem -> newElem) -> List e elem -> List e newElem
  fmap = mapList

instance Applicative (List NonEmpty) where
  pure :: a -> List 'NonEmpty a
  pure x = Cons x Nil

  (<*>) :: List 'NonEmpty (a -> b) -> List 'NonEmpty a -> List 'NonEmpty b
  (<*>) funcs list = flattenList $ fmap (`fmap` list) funcs

instance Monad (List NonEmpty) where
  (>>=) :: List 'NonEmpty a -> (a -> List 'NonEmpty b) -> List 'NonEmpty b
  (>>=) list f = flattenList $ fmap f list 

singleton :: elem -> List NonEmpty elem
singleton x = Cons x Nil

safeHead :: List NonEmpty elem -> elem
safeHead (Cons x _) = x

safeLast :: List NonEmpty elem -> elem
safeLast (Cons x Nil) = x
safeLast (Cons _ xs@(Cons _ _)) = safeLast xs

-- safeHead Nil = error "haskell detects that this is never reached"
-- since Nil constructor always gives a List a Empty

-- Now though we can't define a function that returns either Nil or Cons, the compiler rejects it
-- silly :: Int -> List Int b
-- silly 0 = Nil
-- silly 1 = Cons 1 Nil

-- can however take both and produce a single one
append :: List e elem -> elem -> List NonEmpty elem
append Nil item = Cons item Nil
append (Cons x xs) item = Cons x $ append xs item

reverseList' :: List e1 elem -> List e2 elem -> List (Combine e1 e2) elem
reverseList' Nil acc = acc
reverseList' (Cons x xs) acc = reverseList' xs (Cons x acc)

-- can also preserve the type of b
reverseList :: List e elem -> List e elem
reverseList xs = reverseList' xs Nil 

-- reverseList (Cons x xs) = reverseList xs
-- is detected as an error as reverseList xs is not guaranteed to be Cons

-- derive functor for list
foldList :: (acc -> elem -> acc) -> acc -> List e elem -> acc
foldList _ acc Nil = acc
foldList f acc (Cons x xs) = foldList f (f acc x) xs

foldrList :: (elem -> acc -> acc) -> acc -> List e elem -> acc
foldrList _ acc Nil = acc
foldrList f acc (Cons x xs) = f x (foldrList f acc xs)

safeFold1 :: (elem -> elem -> elem) -> List NonEmpty elem -> elem
safeFold1 f (Cons x xs) = foldList f x xs

mapList :: (elem -> newElem) -> List e elem -> List e newElem
mapList _ Nil = Nil
mapList f (Cons x xs) = Cons (f x) (mapList f xs)

lengthList :: List e elem -> Int
lengthList = foldList (\acc _ -> acc + 1) 0

-- Concating is difficult
-- We use the Combine type function to prove that combining empty with NonEmpty list is NonEmpty
concatList :: List e1 elem -> List e2 elem -> List (Combine e1 e2) elem
concatList Nil xs = xs
concatList xs Nil = xs
concatList (Cons x xs) second = Cons x (concatList xs second)

-- Here we somehow need a way to say that the list can contain nonEmpty and Empty Lists
-- for now this can only work for lists that are all NonEmpty
flattenList :: List NonEmpty (List NonEmpty elem) -> List NonEmpty elem
flattenList (Cons list1 Nil) = list1
flattenList (Cons list1 remaining@(Cons _ _)) = concatList list1 $ flattenList remaining

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
  print $ fmap (+ 2) list
  print $ foldList (*) 1 list
  print $ safeFold1 (*) list
  print $ concatList list $ reverseList list
  print $ lengthList list
  print $ Cons (+1) Nil <*> (pure 1 :: List NonEmpty Int)
  print $ Cons (+1) (Cons (+2) Nil) <*> list
  print $ list >>= (\element -> Cons element (Cons 0 Nil))
  where
    list :: List NonEmpty Int = Cons 5 $ Cons 2 $ Cons 3 Nil
    list2 :: List NonEmpty Int = singleton 10
