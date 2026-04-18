-- 1. Reemplazar todas las occurencias de un valor por otro dado
{-
  reemplazarTodos 1 9 [1,2,1,3]
  → [9,2,9,3]
-}
replaceAll :: (Eq a) => a -> a -> [a] -> [a]
replaceAll _ __ [] = []
replaceAll r n (x : xs)
  | x == r = n : replaceAll r n xs
  | otherwise = x : replaceAll r n xs

-- 2. Filtrar un elemento a partir de
--    funcion :: a -> Bool
--    una lista

filterList :: (a -> Bool) -> [a] -> [a]
filterList f [] = []
filterList f (x : xs)
  | f x = x : filterList f xs
  | otherwise = filterList f xs