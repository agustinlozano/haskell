-- f ) foo6 x y z = x ++ y z

-- Function application has higher precedence

-- z :: a
-- y :: (a -> [b])
-- x :: [b]
-- x ++ y z :: [b]

foo6 :: [b] -> (a -> [b]) -> a -> [b]
foo6 x y z = x ++ y z
