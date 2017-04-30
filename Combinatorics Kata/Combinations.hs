module CombinationsKata where

import Data.List (tails)

-- | List all combinations of length n of a list (6 kyu)
-- | Link: https://biturl.io/Combinations

-- | My original solution
combinations :: Int -> [a] -> [[a]]
combinations n xs =
  filter ((n ==) . length) $ foldr (\a b -> b ++ map (a :) b) [[]] xs

-- | Refactored solution I came up with after completition of this kata
-- more efficient, if a bit longer and verbose
combinations' :: Int -> [a] -> [[a]]
combinations' _ [] = [[]]
combinations' 0 _ = [[]]
combinations' n (x:xs) = xcomb ++ rest
  where
    xcomb = [x : other | other <- combinations (n - 1) xs]
    rest =
      if n <= length xs
        then combinations n xs
        else []

-- | A cleaner and more elegant solution I found on Haskell Wiki
combinations'' :: Int -> [a] -> [[a]]
combinations'' 0 _ = []
combinations'' n xs =
  [y : ys | y:xs' <- tails xs, ys <- combinations'' (n - 1) xs']
