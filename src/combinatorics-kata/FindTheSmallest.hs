module FindTheSmallest where

-- | Form the smallest possible number by moving exactly one digit (5 kyu)
-- | Link: https://biturl.io/Smallest

-- | A refactored solution I came up with after completition of this kata
-- originally my solution used insert and was faster, as it didn't check all
-- permutations, but wasn't 100% right and had edge cases that didn't work
smallest :: Integer -> (Integer, Int, Int)
smallest n = minimum [ (read . f i j . show $ n, i, j) | i <- is, j <- is ]
 where
  is = [0 .. length (show n) - 1]
  f i j xs = let (e, l) = g i xs in h j e l
  g i xs = (xs !! i, take i xs ++ drop (i + 1) xs)
  h i e xs = take i xs ++ [e] ++ drop i xs
