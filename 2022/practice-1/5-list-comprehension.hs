
-- 5) Definir las siguientes funciones usando listas por comprensión:

{- a) 'divisors', que dado un entero positivo 'x' devuelve la
lista de los divisores de 'x' (o la lista vacía si el entero no es positivo) -}

divisores x = if x >= 0
    then [y | y <- [1..x], x `mod` y == 0]
    else []

{- b) 'matches', que dados un entero 'x' y una lista de enteros descarta
de la lista los elementos distintos a 'x' -}
matches :: Eq a => a -> [a] -> [a]
matches x ys = [y | y <- ys, y == x]

{- c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
      '(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,

        donde 0 <= a, b, c, d <= 'n'
-}
-- cuadrupla n = []

{- (d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
'xs' sin elementos repetidos
unique :: [Int] -> [Int] -}

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
