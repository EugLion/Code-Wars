module HaskellSpecificKata where

import Data.Monoid

-- | Implement three basic functions using foldMap: https://biturl.io/FoldMap
-- | my original solution
myToList :: Foldable t => t a -> [a]
myToList = foldMap (:[])

newtype Min a = Min { getMin :: Maybe a }

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  (Min Nothing) `mappend` m = m
  m `mappend` (Min Nothing) = m
  m@(Min (Just x)) `mappend` n@(Min (Just y))
    | x <= y = m
    | otherwise = n

myMinimum :: (Ord a, Foldable t) => t a -> Maybe a
myMinimum xs = getMin $ foldMap (Min . Just) xs

myFoldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
myFoldr f z t = appEndo (foldMap (Endo . f) t) z


-- | Implement two functions to be used with fix: https://biturl.io/FixIt
-- | my original solution
type Fold a b = (a -> b -> b) -> b -> [a] -> b

foldr' :: Fold a b -> Fold a b
foldr' _ _ acc [] = acc
foldr' g f acc (x:xs) = f x (g f acc xs)

reverse' :: ([a] -> [a]) -> [a] -> [a]
reverse' _ [] = []
reverse' g (x:xs) = g xs ++ [x]
