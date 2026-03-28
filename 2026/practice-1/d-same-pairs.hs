{-
    Helper `contains`
    Dada una lista y un elemento verifica si esta contenido
-}

contains :: Eq a => a -> [a] -> Bool
contains _ [] = False
contains n (x:xs)
    | n == x = True
    | otherwise = contains n xs

{-
    Helper `uniques`
    Dada una lista devuleve otra con los valores unicos
-}

uniques :: Eq a => [a] -> [a]
uniques xs = f xs []
    where
        f [] acc = acc
        f (x:xs) acc
            | contains x xs = f xs acc
            | otherwise = f xs (x : acc)

{-
    Helper `frequency`
    Dado un elemento y una lista, devulve la cantidad de veces que aparece el elementos

    frequency :: (Nun a) => e -> [a] -> b
-}
frequency e [] = 0
frequency e xs = f xs 0
    where
        f [] acc = acc
        f (x:xs) acc
            | x == e = f xs (acc + 1)
            | otherwise = f xs acc


{-
    Helper `frequencies`
    Dada una lista mapea una tupla (v,f)
    Donde
        v es un valor unico en lista
        f es la frecuencia del valor en la misma
    
    Tipo de la funcion deberia ser algo asi:
    frequencies :: (Num b) [a] -> (a,b) 
-}

frequencies [] = []
frequencies xs = map (\x -> (x,frequency x xs)) (uniques xs)

{-
    d) paresIguales :: Int → Int → Int → Int → Bool

    toma 4 numeros enteros y retorna True si de dos en dos son iguales (en cualquier orden), 
    en los demas casos retorna False. Por ejemplo:

    paresIguales 3 1 1 2 = False 
    paresIguales 3 1 3 1 = True 
    paresIguales 3 3 1 1 = True
    paresIguales 3 1 1 3 = True
-}

-- paresIguales a b c d = resultado
--   where
--     xs = [a,b,c,d]
--     valores = ...
--     frecs = ...
