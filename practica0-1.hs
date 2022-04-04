-- a)
regla b =
  case b of
    True -> "Quedate en Casa"
    False -> "Qudate en Casa"

-- b)
deletehead [] = []
deletehead [x] = []
deletehead (x:y:xs) = y : deletehead (x:xs)

-- c)
map' f [] =  []
map' f (x:xs) =  f x : map' f xs

-- d)
listNumeros = 1 : 2 : 3 : []

-- e)
[]     ++! ys = ys
(x:xs) ++! ys = x : xs ++! ys


-- f)
addToTail x xs = map (+x) (tail xs)

-- g)
-- listmin xs = (head . sort) xs
