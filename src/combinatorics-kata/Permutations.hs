module Permutations where

import Data.List (delete, nub)

-- | List all permutations of a list (4 kyu)
-- | Link: https://biturl.io/Permutations

-- | My original solution
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = nub [x : ys | x <- xs, ys <- permutations (delete x xs)]
