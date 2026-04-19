-- c) foo3 x y z = x y z

-- Golden Rule: left associative. O sea, (x y) z
-- y :: a
-- x :: (a -> b)
-- x y :: (b -> c) [Esto es necesario porque vemos que x y se aplica inmediatamente a z]
-- z :: b
-- x y z :: c

foo3 :: (a -> b -> c) -> a -> b -> c
foo3 x y z = x y z