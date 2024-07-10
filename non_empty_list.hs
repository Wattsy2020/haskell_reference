{-# LANGUAGE GADTs #-}

data Empty

data NonEmpty

-- the second type y is either Empty or NonEmpty, we can use this to control what types a function accepts
data List x y where
  Nil :: List a Empty
  Cons :: a -> List a b -> List a NonEmpty

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

-- Concating seems very difficult: how to prove that combining empty with NonEmpty list is NonEmpty?
--concatList :: List a b -> List a c -> List a b
--concatList Nil Nil = Nil
--concatList Nil second = second
--concatList (Cons x xs) second = 

concatListEN :: List a Empty -> List a NonEmpty -> List a NonEmpty
concatListEN _ = id

concatListNE :: List a NonEmpty -> List a Empty -> List a NonEmpty
concatListNE nonempty _ = nonempty

concatList :: List a b -> List a b -> List a b
concatList Nil Nil = Nil
concatList (Cons x Nil) second = Cons x second
concatList (Cons x xs@(Cons _ _)) second = Cons x $ concatList xs second

lengthList :: List a b -> Int
lengthList = foldList (\acc x -> acc + 1) 0

-- this one also give errors when trying to implement
-- maybe need full dependent types to work
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
  print $ mapList (+2) list
  print $ foldList (*) 1 list
  print $ safeFold1 (*) list
  print $ concatList list list
  print $ concatListNE list Nil
  print $ lengthList list
  where
    list = Cons 5 $ Cons 2 $ Cons 3 Nil
    list2 = singleton 10