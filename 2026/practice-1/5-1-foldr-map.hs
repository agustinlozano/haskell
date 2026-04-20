{-
    a) map :: (a → b) → [a] → [b]

    Que dada una funcion y una lista, aplica la funcion a cada elemento de la lista.
-}

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f xs = foldr (\x ac -> ...) [] xs