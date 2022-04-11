-- Dado el tipo de dato
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving Show

list = Consnoc 8 (Consnoc 3 (CUnit 9) 2) 33

-- a) Implementar las operaciones de este tipo algebraico:
-- Funciones de acceso

headCL :: CList a -> a
headCL (CUnit a) = a
headCL (Consnoc l xs r) = l

rearmar :: CList a -> a -> CList a
rearmar xs v = case xs of
    EmptyCL          -> CUnit v
    (CUnit a)        -> Consnoc a EmptyCL v
    (Consnoc l ys r) -> Consnoc l (rearmar ys r) v

tailCL :: CList a -> CList a
tailCL (CUnit a) = EmptyCL
tailCL (Consnoc l xs r) = rearmar xs r

isEmptyCL :: CList a -> Bool
isEmptyCL EmptyCL = True
isEmptyCL _ = False

isCUnit :: CList a -> Bool
isCUnit (CUnit a) = True
isCUnit _ = False