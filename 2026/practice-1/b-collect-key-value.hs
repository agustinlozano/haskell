-- helper para poder resolver `collect`
myInsert k v [] = [(k,[v])]
myInsert k v ((k',vs):xs) =
  if k == k' then (k,v:vs) : xs
  else (k',vs) : myInsert k v xs

collect [] = []
collect ((k,v):xs) = myInsert k v (collect xs)