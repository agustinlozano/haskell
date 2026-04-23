data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving (Show)

-- `EmptyCL` Lista vacia
-- `CUnit a` Lista con un solo elemento
-- `Consnoc c | (CList a) a` Lista con un elemento al frente (a), un medio recursivo, y un elemento al fondo (a)

{-
    A) Implementar las operaciones de este tipo algebraico:
    - headCL
    - isEmptyCL
    - isCUnitCL
    - tailCL
-}

headCl :: CList a -> a
headCl (CUnit a) = a
headCl (Consnoc l xs r) = l

isEmptyCl :: CList a -> Bool
isEmptyCl EmptyCL = True
isEmptyCl _ = False -- El guion bajo atrapa CUnit y Consnoc

isCUnit :: CList a -> Bool
isCUnit (CUnit _) = True
isCUnit _ = False

tailCl :: CList a -> CList a
tailCl (CUnit a) = EmptyCL
tailCL (Consnoc l xs r)
  | isEmptyCl xs = CUnit r
  | otherwise = Consnoc (headCl xs) (tailCL xs) r

-- Version optimizada por AI
tailCl' :: CList a -> CList a
tailCl' (Consnoc l EmptyCL r) = CUnit r
tailCl' (Consnoc l (CUnit m) r) = Consnoc m EmptyCL r
tailCl' (Consnoc l (Consnoc m ys n) r) = Consnoc m (tailCL (Consnoc m ys n)) r

{-
    B) Definir una funci´on reverseCL que toma una CList y devuelve su inversa
-}
-- Mi forma
reverseCl :: CList a -> CList a
reverseCl (Consnoc l EmptyCL r) = Consnoc r EmptyCL l
reverseCl (Consnoc l (CUnit m) r) = Consnoc r (CUnit m) l
reverseCl (Consnoc l (Consnoc m ys n) r) = Consnoc r (reverseCl (Consnoc m ys n)) l

-- Simplificada por AI
reverseCl' :: CList a -> CList a
reverseCl' EmptyCL = EmptyCL
reverseCl' (CUnit x) = CUnit x
reverseCl' (Consnoc l xs r) = Consnoc r (reverseCl' xs) l

{-
    C) Definir una funcion inits que toma una CList y
    devuelve una CList con todos los posibles inicios de la CList.
-}

-- helper para tomar un elemento de la CList y pegarlo al final
snocCL :: a -> CList a -> CList a
snocCL x EmptyCL = CUnit x
snocCL x (CUnit y) = Consnoc y EmptyCL x

snocCl x (Consnoc l xs r) = Consnoc l xs (snocCL x r)

inits :: CList a -> CList (CList a)
inits EmptyCL = CUnit EmptyCL
inits (CUnit a) = Consnoc EmptyCL EmptyCL (CUnit a)
inits cl = snocCL cl (inits (initCL cl))
  where
    -- initCL devuelve la CList sin el último elemento
    initCL (CUnit _) = EmptyCL
    initCL (Consnoc l xs r)
      | isEmptyCl xs = CUnit l
      | otherwise = Consnoc l (initCL (snocCL r xs)) (headCl (reverseCl (snocCL r xs)))

-- Nota: initCL es el espejo de tailCL que hicimos antes.