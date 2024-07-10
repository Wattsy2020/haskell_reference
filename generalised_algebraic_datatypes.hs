{-# LANGUAGE GADTs #-}

data Exp a where
  Lam :: (Exp s -> Exp t) -> Exp (s -> t)
  App :: Exp (s -> t) -> Exp s -> Exp t
  Con :: a -> Exp a

intp :: Exp a -> a
intp (Con a) = a
intp (Lam f) = intp . f . Con
intp (App fun arg) = intp fun (intp arg)

lamExample :: Exp Int -> Exp Int
lamExample (Con a) = Con (a + 1)

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

safeHead :: List x NonEmpty -> x
safeHead (Cons a b) = a

-- safeHead Nil = error "haskell detects that this is never reached"
-- since Nil constructor always gives a List a Empty

-- can do normal function things,
-- ensuring we never pass an empty list (whose sum is undefined) to safeSum
safeSum :: List Int NonEmpty -> Int
safeSum (Cons x xs) = case xs of
  Nil -> x
  (Cons x2 xs2) -> x + safeSum xs

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

main :: IO ()
main = do
  print $ intp (Con 1)
  -- the Lam constructor converts a function that takes an Exp a and returns Exp b into an Exp (a -> b)
  -- i.e. an expression whose value is a function
  print $ intp (App (Lam lamExample) (Con 1))
  -- lamExample is a function that only supports Con (a plain value)
  print $ intp $ lamExample (Con 2)
  -- safehead only supports NonEmpty lists
  print $ safeHead list
  print $ safeSum list
  print $ append list 1
  print $ reverseList list
  where
    list = Cons 5 $ Cons 2 $ Cons 3 Nil
