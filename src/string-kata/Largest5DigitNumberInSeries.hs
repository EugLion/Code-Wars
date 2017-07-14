module Largest5DigitNumberInSeries where

-- | From a list of numbers return the biggest 5 digit one (5 kyu)
-- | Link: https://biturl.io/5Digit

-- | My original solution
digit5 :: String -> Int
digit5 [] = 0
digit5 s@(_:xs) = max (read $ take 5 s) (digit5 xs)
