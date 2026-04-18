{-
    Helper to return a list of divisors
-}

divisores n = [m | m <- [1 .. (n - 1)], n `mod` m == 0]

{-
    c) abundantes :: [Integer]

    Lista de todos los numeros abundantes.
    Un numero natural n se denomina abundante si es menor que la suma de sus divisores propios.
    Por ejemplo, 12 y 30 son abundantes pero 5 y 28 no lo son.

    Por ejemplo abundates = [12, 18, 20, 24, 30, 36, ...]
-}

abundantes :: [Integer]
abundantes = [n | n <- [0 .. 100], n < sum (divisores n)]
