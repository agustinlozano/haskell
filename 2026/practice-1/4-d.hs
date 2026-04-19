-- d) foo4 x y z = x y : z

-- Function application has higher precedence
-- y :: a
-- x :: a -> b
-- z :: [b]
-- x y : z :: [b]

foo4 :: (a -> b) -> a -> [b] -> [b]
foo4 x y z = x y : z