{-
    a) map :: (a → b) → [a] → [b]

    Que dada una funcion y una lista, aplica la funcion a cada elemento de la lista.
-}

map' :: (a -> b) -> [a] -> [b]
-- map' f [] = [] // No haría falta xq foldr ya maneja caso base.
map' f xs = foldr (\x acc -> f x : acc) [] xs
