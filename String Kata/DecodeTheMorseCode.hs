module DecodeTheMorseCodeKata where

-- | Decode the Morse code (6 kyu)
-- | Link: https://biturl.io/Morse1

import Data.Map (Map, (!))
import Data.List.Split (splitOn)

-- | Refactored solution I came up with after completition of this kata
-- Originally I used groupBy to split the morse code into words

-- Defined in the Kata, used to decode morse code to letters
morseCodes :: Map String String
morseCodes = undefined

decodeMorse' :: String -> String
decodeMorse' =
  unwords
    . filter (not . null)
    . map (concatMap (morseCodes!) . words)
    . splitOn "   "
