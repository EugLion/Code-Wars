module FixItKata where

import Data.Function (fix)

-- | Implement foldr and reverse with fix (4 kyu)
-- | Link: https://biturl.io/FixIt

-- | My original solution
type Fold a b = (a -> b -> b) -> b -> [a] -> b

foldrFix :: Fold a b -> Fold a b
foldrFix _ _ acc []     = acc
foldrFix g f acc (x:xs) = f x (g f acc xs)

foldr' :: Fold a b
foldr' = fix foldrFix

reverseFix :: ([a] -> [a]) -> [a] -> [a]
reverseFix _ []     = []
reverseFix g (x:xs) = g xs ++ [x]

reverse' :: [a] -> [a]
reverse' = fix reverseFix
