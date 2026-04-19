-- a) foo1 p = if p then (p ∧) else (p ∧)

-- La función devuelve la aplicación parcial de p &&
-- Es decir, una función que espera un predicado para evaluarse a un Bool
-- (Bool -> Bool)
-- p :: Bool

foo1 :: Bool -> (Bool -> Bool)
foo1 p = if p then (p &&) else (p &&)
