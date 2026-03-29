-- Primer try usando el helper `pushForward`
pushFoward _ [] = []
pushFoward n ((i,v):xs) 
    | i > n-1 = v : pushFoward n xs
    | otherwise = pushFoward n xs ++ [v]

{-
    f) ror 
    Dada una lista xs y un entero n, tal que n <= lenght xs, 
    rota los primeros n elementos de xs a la derecha: 
        ror 3 [1, 2, 3, 4, 5] = [4, 5, 1, 2, 3]. 
    Definir una version recursiva de ror, sin usar drop, take ni tail
-}

ror1 _ [] = []
ror1 n xs 
    | n > length xs = []
    | otherwise = pushFoward n (zip [0..] xs)