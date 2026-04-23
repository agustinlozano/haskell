{-
    unzip :: [(a, b)] → ([a], [b])

    Dada una lista de tuplas xs retorna una tupla de listas donde
    cada una corresponde a los primeros y secundos elementos de los pares respectivamente.

    Ej. unzip [(’a’, 1),(’z’, 7),(’h’, 9)] = ("azh", [1, 7, 9])
-}

unzip' :: [(a, b)] -> ([a], [b])
unzip' xs = foldr (\(k, v) (a, b) -> (k : a, v : b)) ([], []) xs
