{-
    5. Si un arbol binario es dado como un nodo con dos subarboles identicos 
    se puede aplicar la tecnica sharing, para que los subarboles sean 
    representados por el mismo arbol. Definir las siguientes funciones de 
    manera que se puedan compartir la mayor cantidad posible de elementos
    de los arboles creados:
-}

data Tree a = Hoja | Nodo (Tree a) a (Tree a) deriving Show


{-
    a) completo :: a → Int → Tree a, tal que dado un valor x de tipo a y 
    un entero d, crea un arbol binario completo de altura d con el valor x 
    en cada nodo.
-}

completo :: a -> Int -> Tree a
completo x 0 = Hoja
completo x d = Nodo (completo x (d-1)) x (completo x (d-1))

-- Ahora la version usando la tecnica de sharing

{- En este caso la recursion es unica, y es la que esta dada por
la expresion `t`. De tal manera que podemos resolver el problema
compartiendo esta recursion para ambos subarboles de nuestro arbol.
-}
completo' :: a -> Int -> Tree a
completo' x 0 = Hoja
completo' x d = let subarbol = completo' x (d-1)
                    in Nodo subarbol x subarbol


{-
    b) balanceado :: a → Int → Tree a, tal que dado un valor x de 
    tipo a y un entero n, crea un arbol binario balanceado de tamanio n, 
    con el valor x en cada nodo.
-}
balanceado :: a -> Int -> Tree a
balanceado x 0 = Hoja
balanceado x n | odd n     = let t = balanceado x (div (n-1) 2)
                                in Nodo t x t
               | otherwise = let m = div (n-1) 2
                                in Nodo (balanceado x m) x (balanceado x (m+1))
-- Llamada con 
--      x = 'a' y n = 2

-- Resultado:
--                     'a'
--                    /   \
--                   H    'a'
--                       /   \
--                      H     H
