{-
    6) El producto escalar de dos listas de enteros de igual longitud
    es la suma de los productos de los elementos sucesivos (misma
    posición) de ambas listas. Definir una función 'scalarProduct' que
    devuelva el producto escalar de dos listas.

    Sugerencia: Usar las funciones 'zip' y 'sum'. 
-}

scalarProduct xs ys = sum [ x*y | (x,y) <- zip xs ys ]