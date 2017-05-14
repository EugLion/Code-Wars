module ConvertStringToCamelCaseKata where

import Data.Char (toUpper)

-- | Given a string of separated words, return them in camelCase (5 kyu)
-- | Link: https://biturl.io/camelCase

-- | My original solution
-- another possibility was to use function splitOneOf
toCamelCase :: String -> String
toCamelCase "" = ""
toCamelCase s =
  let (a, b) = break (`elem`"-_") s in a ++ capFst (toCamelCase $ drop 1 b)
 where
  capFst ""     = ""
  capFst (x:xs) = toUpper x : xs


