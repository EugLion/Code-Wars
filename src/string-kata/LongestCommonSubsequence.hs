module LongestCommonSubsequence where

import Data.List (intersect, maximumBy, subsequences)
import Data.Ord (comparing)

-- | Find the longest common subsequence of two strings (4 kyu)
-- | Link: https://biturl.io/lcs

-- | My original solution
lcs :: String -> String -> String
lcs [] _ = []
lcs _ [] = []
lcs s@(x:xs) t@(y:ys)
  | x == y = x : lcs xs ys
  | otherwise = maximumBy (comparing length) [lcs xs t, lcs s ys]

-- | More elegant solution, making use of some interesting List functions
lcs' :: String -> String -> String
lcs' x y =
  maximumBy (comparing length) (subsequences x `intersect` subsequences y)
