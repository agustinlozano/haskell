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
pot _ 0 = 1
pot n a = n * pot n (a - 1)
-- pot n a = a ^ n

-- i) xor, el operador de disyunción exclusiva
xor :: Bool -> Bool -> Bool 
xor True False = True 
xor False True = True
xor _ _ = False

-- j) max3, que toma tres números enteros y devuelve el máximo entre llos
max3 :: Int -> Int -> Int -> Int
max3 a b c = max a (max b c)

-- k) swap, que toma un par y devuelve el par con sus componentes invertidas
swap :: (a, b) -> (b, a)
swap (a, b) = (b,a)