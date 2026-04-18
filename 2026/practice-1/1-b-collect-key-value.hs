-- helper para poder resolver `collect`
myInsert k v [] = [(k,[v])]
myInsert k v ((k',vs):xs)
    | k == k' = (k,v:vs) : xs
    | otherwise = (k',vs) : myInsert k v xs

collect [] = []
collect ((k,v):xs) = myInsert k v (collect xs)