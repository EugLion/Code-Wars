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

