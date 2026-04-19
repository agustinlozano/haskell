-- h) foo8 a b = if b a then a else []

-- Viendo la función vemos que devuelve una lista [t]

-- a :: [t]
-- b :: [t] -> Bool

foo8 :: [t] -> ([t] -> Bool) -> [t]
foo8 a b = if b a then a else []