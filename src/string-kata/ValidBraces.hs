module ValidBraces where

-- | Check if all braces are valid (4 kyu)
-- | Link: https://biturl.io/ValidBraces

-- | My original solution
validBraces :: String -> Bool
validBraces s = go s 0 0 0
 where
  go [] k l m = all (==0) [k, l, m]
  go (x:xs) k l m
    | any (<0) [k, l, m]
    = False
    | otherwise
    = case x of
      '(' -> go xs (k + 1) l m
      ')' -> go xs (k - 1) l m
      '[' -> go xs k (l + 1) m
      ']' -> go xs k (l - 1) m
      '{' -> go xs k l (m + 1)
      '}' -> go xs k l (m - 1)

-- | More elegant solution
validBraces' :: String -> Bool
validBraces' s = "" == foldr collapse [] s
 where
  collapse '(' (')':xs) = xs
  collapse '{' ('}':xs) = xs
  collapse '[' (']':xs) = xs
  collapse x   xs       = x : xs
