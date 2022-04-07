-- 4) Dar al menos dos ejemplos de funciones que tengan cada uno de los siguientes tipos:
{-
d) Int -> Bool
e) Bool -> (Bool -> Bool)
f) (Int,Char) -> Bool
g) (Int,Int) -> Int
h) Int -> (Int,Int)
i) a -> Bool
j) a -> a
-}

-- a) (Int -> Int) -> Int
apply f = f 1 + 2

-- b) Int -> (Int -> Int)
suma' a b = a + b

-- c) (Int -> Int) -> Int -> Int
-- fc f a = f (a + 1) - 1
-- fc1 g a = div (g a) + a