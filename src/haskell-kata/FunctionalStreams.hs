module FunctionalStreams where

-- | Implement streams, i.e. infinite lists, and their functions (4 kyu)
-- | Link: https://biturl.io/Streams

-- | My original solution
data Stream a =
  a :> Stream a

infixr :>

-- basic functions --
headS :: Stream a -> a
headS (x :> _) = x

tailS :: Stream a -> Stream a
tailS (_ :> xs) = xs

-- functions constructing streams --
repeatS :: a -> Stream a
repeatS x = x :> repeatS x

iterateS :: (a -> a) -> a -> Stream a
iterateS f x = x :> iterateS f (f x)

cycleS :: [a] -> Stream a
cycleS xs = foldr :> (cycleS xs) xs

fromS :: Num a => a -> Stream a
fromS x = x :> fromS (x + 1)

fromStepS :: Num a => a -> a -> Stream a
fromStepS x s = x :> fromStepS (x + s) s

-- functions to manipulate with streams --
foldrS :: (a -> b -> b) -> Stream a -> b
foldrS f (x :> xs) = f x (foldrS f xs)

filterS :: (a -> Bool) -> Stream a -> Stream a
filterS f (x :> xs) =
  let fs = filterS f xs
  in if f x
       then x :> fs
       else fs

takeS :: Int -> Stream a -> [a]
takeS n (x :> xs) =
  if n <= 0
    then []
    else x : takeS (n - 1) xs

dropS :: Int -> Stream a -> Stream a
dropS n acc@(_ :> xs) =
  if n == 0
    then acc
    else dropS (n - 1) xs

splitAtS :: Int -> Stream a -> ([a], Stream a)
splitAtS n st = (takeS n st, dropS n st)

zipWithS :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithS f (x :> xs) (y :> ys) = f x y :> zipWithS f xs ys

zipS :: Stream a -> Stream b -> Stream (a, b)
zipS = zipWithS (,)

-- stream Functor and Applicative instances --
instance Functor Stream where
  fmap f (x :> xs) = f x :> fmap f xs

instance Applicative Stream where
  pure = repeatS
  (<*>) = zipWithS ($)

-- basic streams examples -
fibS :: Stream Integer
fibS = 0 :> 1 :> zipWithS (+) fibS (tailS fibS)

primeS :: Stream Integer
primeS = go $ fromS 2
  where
    go (n :> ns) = n :> go (filterS ((/=) 0 . flip mod n) ns)
