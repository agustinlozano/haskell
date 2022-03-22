-- a)
regla b =
  case b of
    True -> "Quedate en Casa"
    False -> "Qudate en Casa"

-- b)
deletehead [] = []
deletehead [x] = []
deletehead (x:y:xs) = y : deletehead (x:xs)

-- c)
map' f [] =  []
map' f (x:xs) =  f x : map' f xs

-- d)
listNumeros = 1 : 2 : 3 : []

-- e)
[]     ++! ys = ys
(x:xs) ++! ys = x : xs ++! ys

-- f)
addToTail :: Num b => b -> [b] -> [b]
addToTail x xs = map (+x) (tail xs)

-- g)
-- listmin xs = (head . sort) xs

{-
2. Definir las siguientes funciones y determinar su tipo:
-}

-- a) five, que dado cualquier valor, devuelve 5
five :: Num p1 => p2 -> p1
five x = 5

{- b) apply, que toma una función y un valor, y devuelve 
el resultado de aplicar la función al valor dado -}
apply f x = f x

-- c) la funcion identidad
identidad :: Num a => a -> a
identidad x = x

-- d) first, que toma un par ordenado, y devuelve su primera componente
first (a, b) = a

-- e) derive, que aproxima la derivada de una función dada en un punto dado
derive h f x = (f (x+h) - f x) / h

-- f) sign, la función signo
sign a
  | a > 0 = 1
  | a == 0 = 0
  | otherwise = -1

-- g) vabs, la función valor absoluto (usando sign y sin usarla)
vabs a
  | sign a == 1 || a == 0 = a
  | otherwise = -a

vabs' a
  | a >= 0 = a
  | otherwise = -a

{- h) pot, que toma un entero y un número, y devuelve el resultado de
elevar el segundo a la potencia dada por el primero -}
pot n a = a ^ n

-- i) xor, el operador de disyunción exclusiva

