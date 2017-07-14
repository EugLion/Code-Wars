module WeightForWeight where

import Data.Char (digitToInt)
import Data.List (sortBy)
import Data.Ord (comparing)

-- | Sort numbers by the sum of their digits (5 kyu)
-- | Link: https://biturl.io/SortWeight

-- | Refactored solution I came up with after completition of this kata
-- originally I compared the number and digSum separately (see Lesson learned)
orderWeight :: String -> String
orderWeight xs = unwords $ sortBy (comparing digSum) (words xs)
  where
    digSum x = (sum $ map digitToInt x, x)
-- | Lesson learned: When comparing tuples, x1 and x2 are compared first
-- and then if they are equal, y1 and y2 are compared second etc.
