{-
    b) oblongoNumber :: [Int] 
    Genera la lista de los numeros oblongos. 
    Un numero es oblongo si es el producto de dos naturales consecutivos. 
    Por ejemplo, los numeros [2, 6, 12, 20, ...]
-}

oblongoNumber :: [Int]
oblongoNumber = [n*(n+1) | n <- [0..]]