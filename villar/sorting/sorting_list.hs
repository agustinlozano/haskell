-- Quicksort sin listas por extension
qsort::(Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort men ++ [x] ++ qsort may
               where (men,may) = particion x xs

particion :: (Ord a) => a -> [a] -> ([a],[a])
particion x [] = ([],[])
particion x (y:ys) = if y<=x then (y:men,may) else (men,y:may)
                     where (men,may) = particion x ys
------------------------------------------------------------------
-- Quicksort con listas por extension
qsort' [] = []
qsort' [x] = [x]
qsort' (x:t) = qsort' menores ++ [x] ++ qsort' mayores
              where
                  menores = [j | j<-t, j < x]
                  mayores = [j | j<-t, j >= x]

------------------------------------------------------------------
--- Selection Sort
delete y [] = []
delete y l  = [x | x <- l, x /= y]

minimo [x] = x
minimo (x:y:y) = if x < y then minimo (x:t)
                 else minimo (y:t)