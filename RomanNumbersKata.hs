import Data.List (isPrefixOf)
import Test.QuickCheck

-- | The table with Roman / Arabic numeral encodings
codeTable :: [(String, Int)]
codeTable = zip (words "M CM D CD C XC L XL X IX V IV I")
                [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]

-- | Encode to Roman numerals: https://biturl.io/RomNumEnc
-- | a solution I found on CW after solving it rather clunkily myself
toRoman :: Int -> Maybe String
toRoman n | n < 0     = Nothing
          | n == 0    = Just ""
          | otherwise = (chr++) <$> toRoman (n - val)
 where
  (chr, val) = head [ (c, v) | (c, v) <- codeTable, v <= n ]


-- | Decode from Roman numerals: https://biturl.io/RomNumDec
-- | a solution I found on CW after solving it rather clunkily myself
fromRoman :: String -> Maybe Int
fromRoman "" = Just 0
fromRoman s  = case prefixes of
  (x:_) -> let (val, rest) = x in (val+) <$> fromRoman rest
  _ -> Nothing
 where
  prefixes = [ (n, drop (length c) s) | (c, n) <- codeTable, c `isPrefixOf` s ]

-- | Tests
prop_equality :: NonNegative Int -> Bool
prop_equality (NonNegative n) = (toRoman n >>= fromRoman) == Just n

testEverything :: IO ()
testEverything = quickCheck prop_equality
