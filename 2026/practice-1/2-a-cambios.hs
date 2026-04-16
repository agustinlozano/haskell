{-
    cambios : [a] → [Int]
    Dada una lista, devuelve la lista de los indices en que la lista cambia. 
    Es decir, dada la lista s retorna la lista con los i tal que
    si !== si+1
    cambios [1, 1, 1, 3, 3, 1, 1] = [2, 4]
-}

badCambios (x:xs) = go (x:xs) x 0 []
    where
        go [] _ _ acc = reverse acc
        go (y:ys) aux i acc
            | y /= aux = go ys y (i+1) ((i-1):acc)
            | otherwise = go ys y (i+1) acc

-- Version corregida
cambios :: Eq a => [a] -> [Int]
cambios [] = []
cambios (x:xs) = go xs x 0 []
    where
        go [] _ _ acc = reverse acc
        go (y:ys) anterior i acc
            | y /= anterior = go ys y (i+1) (i:acc)   -- Guardamos 'i' que es la posición del cambio
            | otherwise     = go ys anterior (i+1) acc
