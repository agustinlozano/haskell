-- Encuentra el mínimo de una lista
minimum' :: Ord a => [a] -> a
minimum' [x] = x
minimum' (x:xs) = min x (minimum' xs)

-- Elimina la primera ocurrencia de un elemento
deleteOne :: Eq a => a -> [a] -> [a]
deleteOne _ [] = []
deleteOne e (x:xs)
    | e == x    = xs
    | otherwise = x : deleteOne e xs

-- Ordena tomando siempre el mínimo
sort' :: Ord a => [a] -> [a]
sort' [] = []
sort' xs = m : sort' (deleteOne m xs)
    where m = minimum' xs