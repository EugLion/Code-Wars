module SecretStringFromRandomTripletsKata where

import Data.List (nub)

-- | Find word using ordered triplets of chars contained in the word (4 kyu)
-- | Link: https://biturl.io/SecretStr

-- | Refactored solution I came up with after finishing the kata
-- originally I'd split the triplets into pairs and decoded them after that
recoverSecret :: Eq a => [[a]] -> [a]
recoverSecret [] = []
recoverSecret ts = first : recoverSecret rest
  where
    isFst x = all (notElem x . tail) ts
    first = head $ filter isFst (nub $ concat ts)
    rest = filter (not . null) $ map (dropWhile (== first)) ts
