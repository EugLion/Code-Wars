module StringAlgorithmsKata where

import Data.List (delete, nub, maximumBy, subsequences, intersect)
import Data.Ord (comparing)

-- | List all permutations of a list: https://biturl.io/Permutations
-- | my original solution
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = nub [ x : ys | x <- xs, ys <- permutations (delete x xs) ]

-- | Find the lcs of two strings: https://biturl.io/lcs
-- | my original solution
lcs :: String -> String -> String
lcs [] _  = []
lcs _  [] = []
lcs s@(x:xs) t@(y:ys)
  | x == y    = x : lcs xs ys
  | otherwise = maximumBy (comparing length) [lcs xs t, lcs s ys]

-- | a solution I found on CW, which uses some interesting Data.List functions
lcs' :: String -> String -> String
lcs' x y =
  maximumBy (comparing length) (subsequences x `intersect` subsequences y)


-- | Find all the possible observed PINs: https://biturl.io/PINs
-- | a refactored solution I came up with after completition of this kata
typo :: Char -> String
typo x = case x of
  '1' -> "124"
  '2' -> "1235"
  '3' -> "236"
  '4' -> "1457"
  '5' -> "24568"
  '6' -> "3569"
  '7' -> "478"
  '8' -> "57890"
  '9' -> "689"
  '0' -> "80"

getPINs :: String -> [String]
getPINs = mapM typo
