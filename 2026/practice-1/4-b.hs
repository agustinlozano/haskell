-- b) foo2 x y z = x (y z)

-- Definimos de derecha a izquierda
-- z :: a
-- y :: (a -> b)
-- x :: (b -> c)
-- x (y z) :: c

foo2 :: (b -> c) -> (a -> b) -> a -> c
foo2 x y z = x (y z)