module StringAlgorithmsKata where

import Data.List (delete, nub)


-- | List all permutations of a list: https://biturl.io/Permutations
-- | my original solution
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = nub [ x : ys | x <- xs, ys <- permutations (delete x xs) ]
