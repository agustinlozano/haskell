{-
    g) upto :: Int → Int → [Int]
    Que dado dos numeros enteros n y m devuelve la lista:
    [n, n + 1, n + 2, ..., m ]

    - caso n <= m por un lado
    - la lista [ ] por otro

    No usar listas por comprension.
-}

upto :: Int -> Int -> [Int]
upto n m
  | n > m = []
  | otherwise = n : upto (n + 1) m
