-- Funcion head usando operador cons
head' :: [a] -> a
head' [] = error "No se puede utilizar con lista vacia" 
head' (x:_) = x

-- Funcion tail usando operador cons
tail' :: [a] -> [a]
tail' [] = [] 
tail' (_:xs) = xs

-- Listas por comprension con generador dependiente
{-
    > [(x,y) | x <- [1..3], y <- [x..5]]
    >
    > [(1,1),(1,2),(1,3),(1,4),(1,5),
      (2,2),(2,3),(2,4),(2,5),
      (3,3),(3,4),(3,5)]
-}

-- Usando generadore dependientes podemos definir flatten 
-- definida en el preludio.

flatten' :: [[a]] -> [a]
flatten' xss = [x | xs <- xss, x <- xs]

-- Con listas por comprension podemos definir la funcion factors

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

-- A partir de esa funcion podemos definir prime

prime :: Int -> Bool
prime n = factors n == [1, n]