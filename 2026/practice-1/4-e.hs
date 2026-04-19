-- e) foo5 x y z = x : y z

-- Function application has higher precedence

-- z :: a
-- y :: (a -> [b])
-- x :: b
-- (x : y z) :: [b]

foo5 :: b -> (a -> [b]) -> a -> [b]
foo5 x y z = x : y z