{-
    pair2List :: (a, [b]) → [(a, b)]

    Dado un par formado por un valor x y una lista xs convierta
    a la lista xs en una lista de pares, formada con los elementos de xs y x
-}

pair2List :: (a, [b]) -> [(a, b)]
pair2List (x, ys) = foldr (\y acc -> (x, y) : acc) [] ys
