{-
	d) samePairs :: Int -> Int -> Int -> Int -> Bool

	Retorna True si los 4 valores forman exactamente 2 pares iguales,
	sin importar el orden.

	Ejemplos:
	samePairs 3 1 1 2 = False
	samePairs 3 1 3 1 = True
	samePairs 3 3 1 1 = True
	samePairs 3 1 1 3 = True
-}

samePairs :: Eq a => a -> a -> a -> a -> Bool
samePairs a b c d =
    (a == b && c == d && a /= c) ||
    (a == c && b == d && a /= b) ||
    (a == d && b == c && a /= b)
