-- i) foo9 a b = if b a then head (:a) else (:[])

-- 🚫🚩 [It seems there's somthing wrong with this fn]

-- 4-i.hs:3:30: error: [GHC-83865]
--     • Couldn't match expected type: [a2 -> [a2]]
--                   with actual type: a1 -> [a1]
--     • In the first argument of ‘head’, namely ‘(: a)’
--       In the expression: head (: a)
--       In the expression: if b a then head (: a) else (: [])
--     • Relevant bindings include
--         b :: [a1] -> Bool (bound at 4-i.hs:3:8)
--         a :: [a1] (bound at 4-i.hs:3:6)
--         foo9 :: [a1] -> ([a1] -> Bool) -> a2 -> [a2] (bound at 4-i.hs:3:1)
--   |
-- 3 | foo9 a b = if b a then head (: a) else (: [])
--   |                              ^^^

---

-- Asumiendo que la función se espera que devuelva una aplicación parcial (:[]) o (:a)
-- Rama else devuelve (t -> [t])
-- Entonces head (:a) debe ser lo mismo (t -> [t])
-- Para que head someting devuelva una lista, something tiene que ser [[t]]
-- a :: [[t]]
-- :a :: [t] -> [[t]]
-- De esta forma hay un choque de tipos porque la exp del then no puede cumplir con (t -> [t])
