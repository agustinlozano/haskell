{-
    Helper `contains`
    Dada una lista y un elemento verifica si 
    el elemento esta contenido en la lista
-}

contains :: Eq a => a -> [a] -> Bool
contains _ [] = False
contains n (x:xs)
    | n == x = True
    | otherwise = contains n xs

{-
    Helper `uniques`
    Dada una lista devuleve otra con los valores unicos (sin duplicados)
-}

uniques :: Eq a => [a] -> [a]
uniques xs = f xs []
    where
        f [] acc = acc
        f (x:xs) acc
            | contains x acc = f xs acc
            | otherwise = f xs (x : acc)

{-
    Helper `frequency`
    Dado un elemento y una lista, devulve la cantidad de veces 
    que aparece el elemento en la lista
-}

frequency :: Eq a => a -> [a] -> Int
frequency e [] = 0
frequency e xs = f xs 0
    where
        f [] acc = acc
        f (x:xs) acc
            | x == e = f xs (acc + 1)
            | otherwise = f xs acc

{-
    Helper `frequencies`
    Dada una lista mapea otra donde cada elemento es una tupla (v,f)
    Donde:
        v es un valor unico en lista original
        f es la frecuencia del valor en la misma
-}

frequencies :: Eq a => [a] -> [(a,Int)]
frequencies [] = []
frequencies xs = map (\x -> (x,frequency x xs)) (uniques xs)

{-
    d) samePairs :: Int → Int → Int → Int → Bool

    toma 4 numeros enteros y retorna True si de dos en dos son iguales (en cualquier orden), 
    en los demas casos retorna False. Por ejemplo:

    samePairs 3 1 1 2 = False 
    samePairs 3 1 3 1 = True 
    samePairs 3 3 1 1 = True
    samePairs 3 1 1 3 = True
-}

{-
    Mi solución
    Convertir un problema de combinaciones en uno de frecuencias

    Para eso:
    - Tratar los valores como una lista
    - Averiguar los valores únicos en la lista original
    - Averigual la frecuencia con la que aparece cada valor unico en la lista original
    - Finalmente, verificar que se cumplan las condificiones del ejercicio
-}

samePairs :: Eq a => a -> a -> a -> a -> Bool
samePairs a b c d = (length freqs == 2) && (all validFreq freqs)
    where
        xs = [a,b,c,d]
        freqs = frequencies xs
        validFreq (_,f) = f == 2