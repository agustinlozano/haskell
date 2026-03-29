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

import Data.List (sort)

samePairs :: Ord a => a -> a -> a -> a -> Bool
samePairs a b c d =
    let [w,x,y,z] = sort [a,b,c,d]
    in w == x && y == z && x /= y
