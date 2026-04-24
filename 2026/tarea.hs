
Const :: a -> Clist a -> Clist a
Snow :: Clist a-> a -> Clist a

-- Cosas de arboles q no entiendo una goma

data Tree a = E | N (Tree a) a (Tree a) deriving Show

-- E es empty


completo :: a -> Int -> Tree a
completo x 0 = E
-- completo x d = N (Completo x (d-1) x (completo x (d-1)))
completo x d = let t = (completo x (d-1))
                in N t x t

-- balanceado sin sharing
balanceado x 0 = E
balanceado x n | even n = N (balanceado x (div (n-1) 2)) x (balanceado (div n 2))
                | 
-- investigar esa tecnica llamada tupling con estos arboles
