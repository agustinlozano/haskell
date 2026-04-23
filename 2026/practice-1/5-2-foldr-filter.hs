{-
    filter :: (a → Bool) → [a] → [a]
    Dado un predicado y una lista xs, devuelve una lista
    con los elementos de xs que satisfacen el predicado.
-}

filter' :: (a -> Bool) -> [a] -> [a]
-- filter' _ [] = [] // No haría falta xq foldr ya maneja caso base.
filter' f xs = foldr (\x acc -> if f x then x : acc else acc) [] xs