{-  Un arbol binario de busqueda (BTS) es un arbol 
    binario t tal que:
        - Si t es una hoja es un BTS
        - Si t es un Nodo l a r, tanto l como r tienen que ser BTS,
        y se tiene que cumplir que:
            Si y es un valor en algun nodo de l entonces y <= a
            Si y es un valor en algun nodo de r entoces  y > a

                    (5)
                   /   \
                 (3)    (7)
                /  \      \
              (2)  (5)    (8)
-}

-- a) Data def.
data Bin a = Hoja | Nodo (Bin a) a (Bin a) deriving Show


-- b) Define a tree
t = (Nodo (Nodo (Nodo Hoja 2 Hoja) 4 (Nodo Hoja 5 Hoja)) 6 (Nodo (Nodo Hoja 8 Hoja) 8 Hoja))


-- c) Some functions
{-
    Las operaciones minimum, maximum y member (ver mas abajo) 
    toman un tiempo en relacion a la altura del arbol para ejecutarse.
    Sobre arboles mas bajos se ejecutan mas rapido.
-}
minimum' :: Bin a -> a
minimum' (Nodo Hoja x r) = x
minimum' (Nodo l x r) = minimum' l

maximum' :: Bin a -> a
maximum' (Nodo l x Hoja) = x
maximum' (Nodo l x r) = maximum' r

checkBST :: Ord a => Bin a -> Bool
checkBST Hoja = True
checkBST (Nodo Hoja x Hoja) = True
checkBST (Nodo l x Hoja) = checkBST l && (maximum' l <= x)
checkBST (Nodo Hoja x r) = checkBST r && (minimum' r > x)
checkBST (Nodo l x r) = checkBST l && checkBST r && (maximum' l <= x) && (minimum' r > x)


{-
                 (6)
               /     \
             (4)      (8)
            /  \      /
          (2)  (5)   (8)
-}


-- d) Tree traversal (also known as tree search and walking the tree)
inorder Hoja = []
inorder (Nodo l x r) = inorder l ++ [x] ++ inorder r

postorder Hoja = []
postorder (Nodo l x r) = postorder l ++ postorder r ++ [x]

preorder Hoja = []
preorder (Nodo l x r) = x : (preorder l ++ preorder r)


-- e) Member function
{- Vemos que la guarda (a == b) es la que matchea en caso de que 'a' sea miembro -}

isMember a Hoja = False
isMember a (Nodo l b r) | a == b = True
                        | a < b = isMember a l
                        | otherwise = isMember a r


-- f) Common operations

{-
    Para insertar, recorremos el arbol hasta encontrar una hoja, que
    transformamos en un nuevo nodo.
-}
insert a Hoja =                     Nodo Hoja a Hoja
-- Insertar un elemento 'a' en una hoja

insert a (Nodo l b r) | a <= b    = Nodo (insert a l) b r
                      | otherwise = Nodo l b (insert a r)
-- Insertar el elemento 'a' en un arbol


{-
    Para borrar un elemento en un BST primero lo tengo que encontrar:
        - El nodo encontrado tiene hojas como sub arboles
          | z == b = Hoja

        - El nodo encontrado tiene solo un subarbol con datos
          | z == b = r
          | z == b = l

        - El nodo tiene dos subarboles con datos
          | z == b = let y = minimum r
                     in Nodo l y (delete y r)
-}

delete _ Hoja = Hoja

delete z (Nodo Hoja b Hoja) | z == b = Hoja

delete z (Nodo l b r) | z < b  = Nodo (delete z l) b r
                      | z > b  = Nodo l b (delete z r)
                      | z == b = let y = minimum' r
                                 in Nodo l y (delete y r)

