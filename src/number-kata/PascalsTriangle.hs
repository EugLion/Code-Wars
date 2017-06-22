module PascalsTriangle where

-- | Return first n rows of pascal's triangle (4 kyu)
-- | Link: https://biturl.io/Pascal

-- | My original solution
pascalsTriangle :: Int -> [Int]
pascalsTriangle n = concat $ take n $ iterate nextRow [1]
 where
  nextRow r = zipWith (+) (0 : r) (r ++ [0])
