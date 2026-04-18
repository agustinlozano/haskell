{-
    e) euler :: Int → Int

    Tal que euler n es la suma de todos los multiplos de 3 o 5 menores que n.
    Por ejemplo, euler 10 = 23.
    Puedes usar sin definir la funcion sum que suma los elementos de una lista.
-}

euler :: Int -> Int
euler n = sum [x | x <- [1 .. (n - 1)], x `mod` 3 == 0 || x `mod` 5 == 0]