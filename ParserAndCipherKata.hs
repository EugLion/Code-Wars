module ParserAndCipherKata where

import Data.List (isPrefixOf, last, groupBy, sortBy)
import Data.Char (isDigit, digitToInt, ord)
import Data.Ord (comparing)
import Data.Function (on)


-- | The table with Roman / Arabic numeral encodings
codeTable :: [(String, Int)]
codeTable = zip (words "M CM D CD C XC L XL X IX V IV I")
                [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]

-- | Encode to Roman numerals: https://biturl.io/RomNumEnc
-- | a solution I found on CW after solving it rather clunkily myself
toRoman :: Int -> Maybe String
toRoman n | n < 0     = Nothing
          | n == 0    = Just ""
          | otherwise = (ch++) <$> toRoman (n - val)
 where
  (ch, val) = head [ (c, v) | (c, v) <- codeTable, v <= n ]


-- | Decode from Roman numerals: https://biturl.io/RomNumDec
-- | a solution I found on CW after solving it rather clunkily myself
fromRoman :: String -> Maybe Int
fromRoman "" = Just 0
fromRoman s  = case prefixes of
  (x:_) -> let (val, rest) = x in (val+) <$> fromRoman rest
  _ -> Nothing
 where
  prefixes = [ (n, drop (length c) s) | (c, n) <- codeTable, c `isPrefixOf` s ]


-- | Is string a valid ISBN10 number: https://biturl.io/ISBN10Val
-- | a refactored solution I came up with after completition of this kata
validISBN10 :: String -> Bool
validISBN10 s =
  length s == 10
    && all isDigit (take 9 s)
    && (last s == 'X' || isDigit (last s))
    && sum (zipWith value [1, 2 ..] s) `mod` 11 == 0
 where
  value _ 'X' = 100
  value x y   = digitToInt y * x


-- | Group strings which were originally the same: https://biturl.io/CaesarGr
-- | my original solution (using only ord)
caesarSort :: [String] -> [[String]]
caesarSort s = foldr (\a b -> if a `elem` b then b else a : b) []
  $ map (helper . snd) zipped
 where
  zipped = zip s (map base s)
  helper x = foldr (\ (a1, a2) b -> if a2 == x then a1 : b else b) [] zipped
  base s@(x:_) = map (\c -> (ord c - ord x) `mod` 26) s

-- | a solution I found on CW after solving it rather clunkily myself
caesarSortNew :: [String] -> [[String]]
caesarSortNew = groupBy ((==) `on` uniform) . sortBy (comparing uniform)
  where
    uniform s@(x:_) = map (\c -> (ord c - ord x) `mod` 26) s

-- | Lesson learned: groupBy works best with sorted arguments, comparing
-- |   is great for sorting, g `on` f = g (f x) (f y)
