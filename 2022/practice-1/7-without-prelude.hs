{-
7) Sin usar funciones definidas en el
preludio, defina recursivamente las siguientes funciones y
determine su tipo más general:

f) 'cuadrados', que dada una lista de números, devuelva la
lista de sus cuadrados

g) 'longitudes', que dada una lista de listas, devuelve la
lista de sus longitudes

h) 'orden', que dada una lista de pares de números, devuelve
la lista de aquellos pares en los que la primera componente es
menor que el triple de la segunda

i) 'pares', que dada una lista de enteros, devuelve la lista
de los elementos pares

j) 'letras', que dada una lista de caracteres, devuelve la
lista de aquellos que son letras (minúsculas o mayúsculas)

k) 'masDe', que dada una lista de listas 'xss' y un
número 'n', devuelve la lista de aquellas listas de 'xss'
con longitud mayor que 'n' -}

-- a) 'suma', que suma todos los elementos de una lista de números
-- suma :: [Integer] -> Integer
suma :: Num p => [p] -> p
suma [] = 0
suma (x:xs) = x + suma xs


{-
    b) 'alguno', que devuelve True si algún elemento de una
    lista de valores booleanos es True, y False en caso
    contrario
-}
alguno :: [Bool] -> Bool
alguno [] = False
alguno (x:xs) = x || alguno xs


{-
    c) 'todos', que devuelve True si todos los elementos de
    una lista de valores booleanos son True, y False en caso
    contrario
-}
todos :: [Bool] -> Bool
todos [] = True
todos (x:xs) = x && todos xs


{-
    d) 'codes', que dada una lista de caracteres, devuelve la
    lista de sus ordinales
-}
parametro e = orden e "abcdefghijklmnopqrstuvwxyz" 1

orden :: (Ord e, Num n) => e -> [e] -> n -> n
orden e [] posicion = error "El elemento no pertenece al orden"
orden e [x] posicion = posicion
orden e (x:xs) posicion = if e == x
    then posicion
    else orden e xs (posicion+1)

codes :: Num a => [Char] -> [a]
codes [] = []
codes (c:cs) = parametro c : codes cs

{-

    1 : 2 : 3 : 4 : 5 : codes []
    1 : 2 : 3 : 4 : 5 : []
    [1, 2, 3, 4, 5]

-}

{-
    e) 'restos', que calcula la lista de los restos de la
    división de los elementos de una lista de números dada por otro
    número dado
-}

-- resto :: Integer -> Integer -> Integer

-- resto n d

-- restos :: Num a => [a] -> a -> [a]
