module RomanNumeralsEncoder where

-- | Encode to Roman numerals (4 kyu)
-- | Link: https://biturl.io/RomNumEnc

-- | Solution I found on CW after solving it rather clunkily myself
-- originally I used ugly pattern matching and a replace function
codeTable :: [(String, Int)]
codeTable =
  zip
    (words "M CM D CD C XC L XL X IX V IV I")
    [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]

toRoman :: Int -> Maybe String
toRoman n
  | n < 0 = Nothing
  | n == 0 = Just ""
  | otherwise = (ch ++) <$> toRoman (n - val)
  where
    (ch, val) = head [(c, v) | (c, v) <- codeTable, v <= n]
