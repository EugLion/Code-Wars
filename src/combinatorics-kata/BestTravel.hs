module BestTravel where

import Data.Maybe (listToMaybe)

-- | For v towns, find such a way that is the closest to limit m (5 kyu)
-- | Link: https://biturl.io/travel

-- | Refactored solution I came up with after completition of this kata
-- originally the solution was almost the same
chooseBestSum :: Int -> Int -> [Int] -> Maybe Int
chooseBestSum m v ts = listToMaybe [foldr (step . sum) 0 (combinations v ts)]
  where
    step a b =
      if b < a && a <= m
        then a
        else b
    combinations 0 _ = [[]]
    combinations n xs =
      [ y : ys
      | (y, i) <- zip xs [0 .. (length xs - 1)]
      , ys <- combinations (n - 1) (drop (i + 1) xs)
      ]
