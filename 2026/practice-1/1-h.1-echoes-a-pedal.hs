{-
    Helper repeatChar
-}

repeatChar :: Char -> Int -> [Char]
repeatChar char 0 = []
repeatChar char n = char : repeatChar char (n-1)

{-
    Sin usar `zipWith` ni `replicate`
-}

echo :: [Char] -> [Char]
echo xs = aux 1 xs
  where
    aux i [] = ""
    aux i (c:cs) = repeatChar c i ++ aux (i + 1) cs