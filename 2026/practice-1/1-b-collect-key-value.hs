-- helper para poder resolver `collect`
myInsert :: (Eq t) => t -> t1 -> [(t, [t1])] -> [(t, [t1])]
myInsert k v [] = [(k, [v])]
myInsert k v ((k', vs) : xs)
  | k == k' = (k, v : vs) : xs
  | otherwise = (k', vs) : myInsert k v xs

collect :: (Eq t) => [(t, t1)] -> [(t, [t1])]
collect [] = []
collect ((k, v) : xs) = myInsert k v (collect xs)