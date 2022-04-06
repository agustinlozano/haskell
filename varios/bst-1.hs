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
data Bin a = Hoja | Nodo (Bin a) a (Bin a) deriving Show

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

t = (Nodo (Nodo (Nodo Hoja 2 Hoja) 4 (Nodo Hoja 5 Hoja)) 6 (Nodo (Nodo Hoja 8 Hoja) 8 Hoja))

{-
                 (6)
               /     \
             (4)      (8)
            /  \      /
          (2)  (5)   (8)
-}

inorder Hoja = []
inorder (Nodo l x r) = inorder l ++ [x] ++ inorder r

postorder Hoja = []
postorder (Nodo l x r) = postorder l ++ postorder r ++ [x]

preorder Hoja = []
preorder (Nodo l x r) = x : (preorder l ++ preorder r)
