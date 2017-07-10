module DeleteNth where

import Data.List

-- | Delete every element after its nth occurence
-- | Link: https://biturl.io/nth

-- | My original solution
deleteNth :: [Int] -> Int -> [Int]
deleteNth xs n = foldl (\b a -> if count a b >= n then b else b ++ [a]) [] xs
 where
  count a = length . filter (==a)

-- | A cool solution using group
deleteNth' :: [Int] -> Int -> [Int]
deleteNth' lst n =
  reverse $ (reverse lst) \\ (concatMap (drop n) . group $ sort lst)
