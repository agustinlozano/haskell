{-
    f ) expandir :: [Int] → [Int] 
    
    Reemplace en una lista de numeros positivos cada numero n por n copias de sı mismo:

    Ejemplo: expandir [3, 4, 2] = [3, 3, 3, 4, 4, 4, 4, 2, 2]
-}

-- expandir :: [Int] -> [Int]
expandir xs = [x | x <- xs, x <- replicate x x]
