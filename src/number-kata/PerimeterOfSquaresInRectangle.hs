module PerimeterOfSquaresInRectangle where

-- | Get the sum of perimeters of fibonacci squares (5 kyu)
-- | Link: https://biturl.io/FibRect

-- | My original solution (using only ord)
perimeter :: Integer -> Integer
perimeter n = 4 * sum (take (1 + fromInteger n) fibs)
 where
  fibs = 1 : 1 : zipWith (+) fibs (tail fibs)


