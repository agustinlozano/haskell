{-
    d) eco 
    
    Devuelve la cadena obtenida a partir de la cadena xs repitiendo cada elemento tantas
    veces como indica su posicion. 
    
    Por ejemplo: eco "hola" = "hoolllaaaa"
-}

eco :: [Char] -> [Char]
eco str = concat [replicate i v | (v,i) <- zip str [1..]]

-- Usando listas por compresion anidadas
eco2 :: [Char] -> [Char]
eco2 str = [ x | (v, i) <- zip str [1..], x <- replicate i v ]