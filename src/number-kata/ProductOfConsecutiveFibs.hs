module ProductOfConsecutiveFibs where

-- | Say if a number is a product of two consecutive fibs (5 kyu)
-- | Link: https://biturl.io/ProductFib

-- | Refactored solution I came up with after finishing the kata
-- originally the solution wasn't as concise, but worked the same
productFib :: Integer -> (Integer, Integer, Bool)
productFib n = go n 0 1
  where
    go x a b
      | x >= a * b = (a, b, x == a * b)
      | otherwise = go x b (a + b)
