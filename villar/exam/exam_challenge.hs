{-
  a) Escribir una función que recibe como argumento dos 
  listas ordenadas, y devuelve una lista ordenada fusión 
  de las listas argumentos (No se debe usar ningún método 
  de clasificación).

  juntar :: (Ord a)=> [a]->[a]->[a]
-}
juntar :: (Ord a) => [a]->[a]->[a]
juntar xs [] = xs
juntar [] ys = ys
juntar (x:xs) (y:ys) = if x < y then x:juntar xs (y:ys)
                       else y:juntar (x:xs) ys
