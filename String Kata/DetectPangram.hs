module DetectPangramKata where

import Data.Char (toLower)

-- | Check if string is a pangram, i.e. contains every letter (6 kyu)
-- | Link: https://biturl.io/Pangram

-- | Refactored solution I came up with after completition of this kata
-- originally the solution was almost the same
isPangram :: String -> Bool
isPangram s = all (`elem` map toLower s) ['a' .. 'z']
