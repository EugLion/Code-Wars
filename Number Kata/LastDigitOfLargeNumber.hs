module LastDigitOfLargeNumberKata where

-- | Get the last digit of the number x^y (5 kyu)
-- | Link: https://biturl.io/Last

-- | Refactored solution I came up with after completition of this kata
-- originally my solution wasn't as elegant, but was based on similar principle
lastDigit :: Integer -> Integer -> Integer
lastDigit _ 0 = 1
lastDigit x y = ((x `rem` 10) ^ ((y `rem` 4) + 4)) `rem` 10
