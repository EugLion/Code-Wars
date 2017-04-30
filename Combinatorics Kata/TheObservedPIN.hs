module TheObservedPINKata where

-- | Find all the possible PINs - number can be any of its neighbours (4 kyu)
-- | Link: https://biturl.io/PINs

-- | Refactored solution I came up with after completition of this kata
-- originally I didn't use mapM, but (sequence $ map typo s)
typo :: Char -> String
typo x =
  case x of
    '1' -> "124"
    '2' -> "1235"
    '3' -> "236"
    '4' -> "1457"
    '5' -> "24568"
    '6' -> "3569"
    '7' -> "478"
    '8' -> "57890"
    '9' -> "689"
    '0' -> "80"

getPINs :: String -> [String]
getPINs = mapM typo
