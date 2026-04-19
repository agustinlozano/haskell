-- g) foo7 a b = if b a then head a else []

-- En el `else` vemos una lista, así que foo7 debe devolver una lista [t]

-- head a :: [t]
-- b a :: Bool
-- a :: [[t]]
-- b :: [[t]] -> Bool

foo7 :: [[t]] -> ([[t]] -> Bool) -> [t]
foo7 a b = if b a then head a else []