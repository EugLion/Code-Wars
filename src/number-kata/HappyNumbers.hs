module HappyNumbers where

import Data.Char (digitToInt)

-- | Is number a "happy" number (5 kyu)
-- | Link: https://biturl.io/HappyNum

-- | Refactored solution I came up with after completition of this kata
-- originally I stored all preceding numbers - Wikipedia helped me tremendously
isHappy :: Int -> Bool
isHappy 1 = True
isHappy 4 = False
isHappy n = isHappy $ sum $ map ((^2) . digitToInt) (show n)
