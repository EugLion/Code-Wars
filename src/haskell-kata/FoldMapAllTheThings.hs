module FoldMapAllTheThings where

import Data.Monoid

-- | Implement toList, minimum and foldr using foldMap (4 kyu)
-- | Link: https://biturl.io/FoldMap

-- | My original solution
myToList :: Foldable t => t a -> [a]
myToList = foldMap (: [])

newtype Min a = Min
  { getMin :: Maybe a
  }

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
