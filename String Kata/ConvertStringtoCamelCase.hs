module ConvertStringToCamelCaseKata where

import Data.Char (toUpper)

-- | Given a string of separated words, return them in camelCase (5 kyu)
-- | Link: https://biturl.io/camelCase

-- | My original solution
-- another possibility was to use function splitOneOf
toCamelCase :: String -> String
toCamelCase "" = ""
toCamelCase xs =
  let (a, b) = break (flip elem "-_") xs
  in a ++ (capFst $ toCamelCase $ drop 1 b)
  where
    capfst "" = ""
    capFst (x:xs) = toUpper x : xs

