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
pot _ 0 = 1
pot n a = n * pot n (a - 1)
-- pot n a = a ^ n

-- i) xor, el operador de disyunción exclusiva
-- xor a
--   |
--   | otherwise 

-- j) max3, que toma tres números enteros y devuelve el máximo entre llos

-- k) swap, que toma un par y devuelve el par con sus componentes invertidas

{-
3) Definir una función que determine si un año es bisiesto o no, de
acuerdo a la siguiente definición:

año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

¿Cuál es el tipo de la función definida?
-}

bisiesto a = (mod a 4 == 0) && not (mod a 100 == 0) || (mod a 400 == 0)


-- 4) Dar al menos dos ejemplos de funciones que tengan cada uno de los siguientes tipos:
{-
a) (Int -> Int) -> Int


d) Int -> Bool
e) Bool -> (Bool -> Bool)
f) (Int,Char) -> Bool
g) (Int,Int) -> Int
h) Int -> (Int,Int)
i) a -> Bool
j) a -> a
-}

-- b) Int -> (Int -> Int)
fb a b = a + b

-- c) (Int -> Int) -> Int -> Int
-- fc f a = f (a + 1) - 1
-- fc1 g a = div (g a) + a

{-
(d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
'xs' sin elementos repetidos
unique :: [Int] -> [Int]
-}

{-
  [1, 2, 3, 2, 1, 7, 3] -> [1, 2, 3, 7]
  
  1:[2, 3, 2, 1, 7, 3]
      <descarto el 1 porque esta en la cola de la lista>
  2:[3, 2, 1, 7, 3]
      <descarto el 2 porque esta en la cola de la lista>
  3:[2, 1, 7, 3]
      <descarto el 3 porque esta en la cola de la lista>
  2:[1, 7, 3]

-}

unicos [] ls = ls
unicos (x:xs) ls = if elem x xs
  then unicos xs ls
  else unicos xs (x:ls)

unicos' [] = []
unicos' (x:xs) = if elem x xs
  then unicos' xs
  else x: unicos' xs

-- unique xs = [x | (x, i) <- zip xs [1..], not (elem x (drop i xs))]

{-
7) Sin usar funciones definidas en el
preludio, defina recursivamente las siguientes funciones y
determine su tipo más general:

d) 'codes', que dada una lista de caracteres, devuelve la
lista de sus ordinales
-}

-- code' elemento [x] n = n
-- code' elemento (x:xs) n = if elemento == x
--   then n
--   else code' elemento xs (n+1)

{-
h) 'orden', que dada una lista de pares de números, devuelve
la lista de aquellos pares en los que la primera componente es
menor que el triple de la segunda


-}

{-
i) 'pares', que dada una lista de enteros, devuelve la lista
de los elementos pares


-}


{-
8) Redefinir las funciones del ejercicio anterior usando foldr, map y filter.
ver su definición en https://hoogle.haskell.org/


-}

-- funciones para introducir la utilidad de foldr
-- el foldr trata de capturar esta estructura
and' [] = True
and' (b:bs) = b && and' bs

or' [] = False
or' (b:bs) = b || or' bs

sum' [] = 0
sum' (x:xs) = x + sum' xs

-- Ahora hagamos nuestra propia implementacion de la funcion foldr
foldr' f e [] = e
foldr' f e (x:xs) = x `f` (foldr' f e xs)

-- entoces el nuevo and, llamado and2 ahora queda
and2 xs = foldr' (&&) True xs
-- entoces el nuevo and, llamado and2 ahora queda
sum2 xs = foldr' (+) 0 xs

{-
  Ejercicio para entragar:
  Definir la funcion map usando foldr

  map' f xs = foldr...
-}