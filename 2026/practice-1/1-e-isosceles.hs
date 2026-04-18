{-
    e) isosceles :: Int → Int → Int → Bool
    que dadas la longitud de los lados de un triangulo nos dice si es un triangulo isosceles
-}

badIsosceles :: Int -> Int -> Int -> Bool
badIsosceles a b c
  | a == b = True
  | b == c = True
  | c == a = True
  | otherwise = False

-- La solucion de arriba esta mal porque no corrobora que
-- La suma de ambos lados iguales sean mayor en long al tercel lado

isosceles :: Int -> Int -> Int -> Bool
isosceles a b c
  | a == b && a + b > c = True
  | b == c && b + c > a = True
  | a == c && a + c > b = True
  | otherwise = False
