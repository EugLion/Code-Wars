module RangeExtraction where

import Data.Function (on)
import Data.List (groupBy, intercalate)

-- | Return the given ints as s String, including ranges (4 kyu)
-- | Link: https://biturl.io/Range

-- | My original solution
range :: [Integer] -> String
range xs = intercalate "," . map go $ groupSucc xs
  where
    go a =
      if length a > 2
        then show (head a) ++ "-" ++ show (last a)
        else intercalate "," $ map show a

groupSucc :: [Integer] -> [[Integer]]
groupSucc [] = []
groupSucc xs =
  let (ys, zs) = go xs
  in ys : groupSucc zs
  where
    go [x] = ([x], [])
    go (x:y:ys) =
      if y == succ x
        then let r = go (y : ys)
             in (x : fst r, snd r)
        else ([x], y : ys)

-- | Some cool pointfree stuff
range' :: [Int] -> String
range' =
  intercalate "," . map toRange . groupBy ((==) `on` uncurry (-)) . zip [1 ..]
  where
    toRange [(_, x)] = show x
    toRange [(_, x), (_, y)] = show x ++ ',' : show y
    toRange ((_, x):xs) = show x ++ '-' : show (snd $ last xs)

-- | And the best solution I've seen - clear, concise and readable
range'' :: [Int] -> String
range'' = intercalate "," . map formatRange . toRanges []
  where
    toRanges rs [] = reverse rs
    toRanges [] (x:xs) = toRanges [(x, x)] xs
    toRanges ((a, b):rs) (x:xs) =
      if x == b + 1
        then toRanges ((a, x) : rs) xs
        else toRanges ((x, x) : (a, b) : rs) xs
    formatRange (a, b)
      | a == b = show a
      | a + 1 == b = show a ++ "," ++ show b
      | otherwise = show a ++ "-" ++ show b
