{-
    maxSec :: [(Int, Int)] → (Int, Int)

    Dada una lista de pares de naturales que represente a una lista de segmentos de la recta,
    calcule el segmento mas largo de la misma.

    Ej.: maxSec [(1, 2),(0, 7),(4, 6)] = (0, 7)

    Puede definir una funcion auxiliar
    maxL :: (Int, Int) → (Int, Int) → (Int, Int),
    Que dados dos pares de naturales que representan a dos segmentos de la recta, devuelva el segmento cuya
    longitud sea maxima.

    Ej.: maxL (1, 2) (0, 7) = (0, 7)
-}

maxL :: (Int, Int) -> (Int, Int) -> (Int, Int)
maxL (a, b) (c, d)
  | abs (b - a) > abs (d - c) = (a, b)
  | otherwise = (c, d)

maxSec :: [(Int, Int)] -> (Int, Int)
maxSec [] = (0, 0) -- lista vacía devulve un segmento vacío
maxSec (x : xs) = foldr maxL x xs