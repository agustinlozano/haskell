-- collect :: [(k, v)] -> [(k, [v])]
-- collect [] = []

myInsert k v [] = [(k,[v])]
myInsert k v ((k',vs):xs) =
  if k == k' then (k,v:vs) : xs
  else (k',vs) : myInsert k v xs
