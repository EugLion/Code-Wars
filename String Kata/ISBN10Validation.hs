module ISBN10ValidationKata where

import Data.Char (isDigit, digitToInt)

-- | Check if string is a valid ISBN10 number (5 kyu)
-- | Link: https://biturl.io/ISBN10Val

-- | Refactored solution I came up with after completition of this kata
-- originally the solution was almost the same
validISBN10 :: String -> Bool
validISBN10 s =
  length s == 10 &&
  all isDigit (take 9 s) &&
  (last s == 'X' || isDigit (last s)) &&
  sum (zipWith value [1,2 ..] s) `mod` 11 == 0
  where
    value _ 'X' = 100
    value x y = digitToInt y * x
