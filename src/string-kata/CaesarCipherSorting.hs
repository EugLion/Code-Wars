module CaesarCipherSorting where

import Data.List (groupBy, sortBy, nub)
import Data.Char (ord)
import Data.Ord (comparing)
import Data.Function (on)

-- | Group strings which were, before being coded, originally the same (5 kyu)
-- | Link: https://biturl.io/CaesarGr

-- | My original solution (using only ord)
caesarSort :: [String] -> [[String]]
caesarSort s = nub $ map (helper . snd) zipped
 where
  zipped = zip s (map base s)
  helper x = foldr (\(a1, a2) b -> if a2 == x then a1 : b else b) [] zipped
  base z@(x:_) = map (\c -> (ord c - ord x) `mod` 26) z

-- | More elegant and altogether better solution using groupBy and sortBy
caesarSortNew :: [String] -> [[String]]
caesarSortNew = groupBy ((==) `on` uniform) . sortBy (comparing uniform)
 where
  uniform s@(x:_) = map (\c -> (ord c - ord x) `mod` 26) s

-- | Lessons learned: groupBy works best with sorted arguments,
-- and g `on` f = g (f x) (f y)
