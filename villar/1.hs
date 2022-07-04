{-
  1) Un entero positivo es perfecto si es igual a la suma de sus divisores, 
  excluyendo el propio número. Definir por comprensión la función
-}
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

perfectos :: Int -> [Int]
perfectos n = [x | x <- [1..n], sum(init(divisores x)) == x]

{-
  2) Escribir una función que recibe como argumento dos listas ordenadas, 
  y devuelve una lista ordenada fusión de las listas argumentos 
  (No se debe usar ningún método de clasificación)
-}
juntar :: (Ord a) => [a]->[a]->[a]
juntar xs [] = xs
juntar [] ys = ys
juntar (x:xs) (y:ys) = if x < y then x:juntar xs (y:ys)
                       else y:juntar (x:xs) ys

{-
  3) Escribir una función que inserta elementos en una lista de manera 
  de mantenerla ordenada de menor a mayor. De esta forma cada operación 
  Head sobre la lista devuelve el elemento más chico almacenado en ella.
-}
inserta :: (Ord a) => a->[a]->[a]
inserta elm [] = [elm]
inserta elm (x:xs) = if elm <= x then elm:x:xs
                        else x:(inserta elm xs)

{- 
  4) Consideremos la siguiente función

  split :: (Ord a) => a->[a]->([a],[a])
  split x l = ([y | y <- l, y<=x] , [y | y <- l, y>x])

  Defina una versión de esta función que trabaje en 
  exactamente una sola pasada a la lista "l"
-}
particion :: Ord a => a -> [a] -> ([a],[a])
particion x [] = ([],[])
particion x (y:ys) = if y<=x then (y:men,may) else (men,y:may)
                     where (men,may) = particion x ys

--------------------------------
split' :: Ord a => [a] -> ([a],[a])
split' [] = ([],[])
split' [a] = ([a],[])
split' (a:b:t) = let
                  (m,n) = split' t
                  in
                  (a:m, b:n)

{-
  5) Escriba una función Qsort::(Ord a)=>[a]->[a]. Sin utilizar 
  listas por comprensión.

  Observacion: Escriba una funcion particion que reciba como argumento,
  un valor de referencia o pivot y a una lista de valores del mismo 
  tipo que el pivot. Esta funcion da como resultado una tupla con dos 
  listas (l1 , l2) ... de modo  que en l1 estan todos los valores de 
  la lista original que son menores o iguales que el pivot y en l2 todos 
  los mayores que el pivot.
-}
qsort::(Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort men ++ [x] ++ qsort may
               where (men,may) = particion x xs 

{-
  6) Recordemos que la funcion de la biblioteca ZIP, recibe como 
  argumentos dos listas (x:xs) e (y:ys) y produce una lista de 
  tuplas (i,j) donde los i provienen de la primera lista y los 
  j de la segunda. Cuando una lista es mas larga que la otra, 
  el tesultado contempla solo los pared hasta donde pudieron formarse...

  Ej zip[1,2,3][10..] = [(1,10),(2,11),(3,12)]

  a) Escriba una version personal de la funcion zip, llamada miZip
  
  b) Utilizando miZip y listas por comprension, escriba una funcion 
  que realice el producto escalar de dos listas. Donde producto escalar 
  estaria definido como la suma de los productos uno a uno, componente 
  a componente de cada lista. Si una lista tuviera mas elementos que 
  la otra, al agotarde uno de los operandos se detiene la suma
-}

-- a)
mizip' :: [a]->[b]->[(a,b)]
mizip' [] ys = []
mizip' xs [] = []
mizip' (x:xs) (y:ys) = (x, y) : mizip' xs ys

-- b)
{-
  > mizip' ['A', 'u', 't', 'n', '3', '*'] ['g', 's', 'i', '0']
  > [('A','g'),('u','s'),('t','i'),('n','0')]


  > pescalar [2, 4, 6] [1, 1, 1, 0, 1]
  > 2*1 + 4*1 + 6*1
-}

mult_componentes :: [Float] -> [Float] -> [Float]
mult_componentes xs ys = [a*b | (a, b) <- (mizip' xs ys)]

suma :: [Float] -> Float
suma [] = 0
suma (x:xs) = x + suma xs

p_escalar :: [Float] -> [Float] -> Float
p_escalar xs ys = suma (mult_componentes xs ys)
