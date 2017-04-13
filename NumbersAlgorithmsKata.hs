module NumberAlgorithmsKata where

import Data.List (sortBy)
import Data.Char (digitToInt)
import Data.Ord (comparing)


-- | Sort numbers by the sum of their digits: https://biturl.io/SortWeight
-- | a refactored solution I came up with after completition of this kata
sortBySum :: String -> String
sortBySum xs = unwords $ sortBy (comparing digSum) (words xs)
 where
  digSum x = (sum $ map digitToInt x, x)
-- Lesson learned: When comparing tuples, x1 and x2 are compared first
-- |   and then if they are equal, y1 and y2 are compared second


-- | Is number a "happy" number: https://biturl.io/HappyNum
-- | a refactored solution I came up with after completition of this kata
isHappy :: Int -> Bool
isHappy 1 = True
isHappy 4 = False
isHappy n = isHappy $ sum $ map ((^2) . digitToInt) (show n)


-- | Form the smallest number by moving one digit: https://biturl.io/Smallest
-- | a refactored solution I came up with after completition of this kata
smallest :: Integer -> (Integer, Int, Int)
smallest n = minimum [ (read . f i j . show $ n, i, j) | i <- is, j <- is ]
 where
  is = [0 .. length (show n) - 1]
  f i j xs = let (e, l) = g i xs in h j e l
  g i xs = (xs !! i, take i xs ++ drop (i + 1) xs)
  h i e xs = take i xs ++ [e] ++ drop i xs
-- | my original solution used 'insert' and didn't chceck all permutations,
-- |   but it didn't work 100% of the time, so this solution is still better

-- | Get the last digit of the expression x^y: https://biturl.io/Last
-- | a refactored solution I came up with after completition of this kata
lastDigit :: Integer -> Integer -> Integer
lastDigit _ 0 = 1
lastDigit x y = ((x `rem` 10) ^ ((y `rem` 4) + 4)) `rem` 10
