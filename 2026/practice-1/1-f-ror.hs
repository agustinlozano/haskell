-- First try using `pushForward` helper
pushFoward1 _ [] = []
pushFoward1 n ((i, v) : xs)
  | i > n - 1 = v : pushFoward1 n xs
  | otherwise = pushFoward1 n xs ++ [v]

-- The issue here happens bc ++ operator is executed each iteration
-- Due the operator has O(n) complexity (linear growth) that aint good ^^

{-
    f) ror
    Dada una lista xs y un entero n, tal que n <= lenght xs,
    rota los primeros n elementos de xs a la derecha:
        ror 3 [1, 2, 3, 4, 5] = [4, 5, 1, 2, 3].
    Definir una version recursiva de ror, sin usar drop, take ni tail
-}

ror1 _ [] = []
ror1 n xs
  | n > length xs = []
  | otherwise = pushFoward1 n (zip [0 ..] xs)

-- 2th try making the algorith faster
-- This approach only use ++ operator once 🧐

pushFoward _ [] = []
pushFoward n items =
  [v | (i, v) <- items, i > n - 1] ++ [v | (i, v) <- items, i <= n - 1]

ror _ [] = []
ror n xs
  | n > length xs = []
  | otherwise = pushFoward n (zip [0 ..] xs)