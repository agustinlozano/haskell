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

or2 xs = foldr' (||) False xs

{-
  Ejercicio para entragar:
  Definir la funcion map usando foldr

  map' f xs = foldr...
-}