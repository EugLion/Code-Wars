module RomanNumeralsDecoder where

import Data.List (isPrefixOf)

-- | Decode from Roman numerals (4 kyu)
-- | Link: https://biturl.io/RomNumDec

-- | Solution I found on CW after solving it rather clunkily myself
-- originally I used ugly pattern matching and a replace function
codeTable :: [(String, Int)]
codeTable =
  zip
    (words "M CM D CD C XC L XL X IX V IV I")
    [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]

fromRoman :: String -> Maybe Int
fromRoman "" = Just 0
fromRoman s =
  case prefixes of
    ((rest, val):_) -> (val +) <$> fromRoman rest
    _ -> Nothing
  where
    prefixes = [(drop (length c) s, n) | (c, n) <- codeTable, c `isPrefixOf` s]
