{-
    Helper repeatChar
-}

repeatChar :: Char -> Int -> [Char]
repeatChar char 0 = []
repeatChar char n = char : repeatChar char (n-1)


{-
    eco 
    que devuelve la cadena obtenida a partir de la cadena xs repitiendo cada elemento tantas
    veces como indica su posicion. 
    No usar listas por comprension.

    Por ejemplo: eco "hola" = "hoolllaaaa"
-}

echo :: [Char] -> [Char]
echo "" = ""
echo cs = concat (zipWith (\i char -> repeatChar char i) [1..] cs)

{-
    Otra forma mas simple de hacerlo es mediante la funcion estandar `replicate`
    en vez de usar mi helper
-}

echo2 cs = concat (zipWith replicate [1..] cs)