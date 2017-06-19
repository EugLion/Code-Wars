module DecodeTheMorseCodeKata where

-- | Decode the Morse code (6 kyu)
-- | Link: https://biturl.io/Morse1

import Data.Map (Map, (!))
import Data.List (groupBy)
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

-- | Another nice solution which uses only groupBy
decodeMorse :: String -> String
decodeMorse = unwords . words . concatMap go . groupBy (\x y -> y /= ' ')
 where
  go x = if x == " " then x else morseCodes ! (trim x)
  trim = f . f where f = reverse . dropWhile (==' ')
