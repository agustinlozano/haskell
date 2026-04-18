{-
    borrarLast
    Que dada una lista borra el ultimo elemento de la misma.
    No utilizar reverse, ni tail.
-}

deleteLast :: [a] -> [a]
deleteLast [] = []
deleteLast [x] = []
deleteLast (x : xs) = x : deleteLast xs
