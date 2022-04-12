-- Dado el tipo de dato
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving Show

list1 = Consnoc 8 (Consnoc 3 (CUnit 9) 2) 33
list2 = Consnoc 7 (Consnoc 5 EmptyCL 1) 2
list3 = Consnoc 3 (Consnoc 11 EmptyCL 8) 33
list4 = Consnoc 11 (CUnit 8) 33
list5 = Consnoc 3 EmptyCL 8
list6 = Consnoc 7 (Consnoc 5 EmptyCL 1) 2

-- a) Implementar las operaciones de este tipo algebraico:
-- Funciones de acceso
headCL :: CList a -> a
headCL EmptyCL = error "No rompas las bolas con meter mierdas raras"
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

-- b) b) Definir una funcion reverseCL que toma una CList y devuelve su inversa.
reverseCL :: CList a -> CList a
reverseCL EmptyCL = EmptyCL
reverseCL (CUnit a) = CUnit a
reverseCL (Consnoc l xs r) = Consnoc r (reverseCL xs) l

{-
    c) Definir una funcion inits que toma una CList y devuelve una CList con 
    todos los posibles inicios de la CList.
-}
cons :: a -> CList a -> CList a
cons x EmptyCL          = CUnit x
cons x (CUnit a)        = Consnoc x EmptyCL a
cons x (Consnoc l xs r) = Consnoc x (cons l xs) r

snoc :: CList a -> a -> CList a
snoc EmptyCL x          = CUnit x
snoc (CUnit y) x        = Consnoc y EmptyCL x
snoc (Consnoc l xs r) x = Consnoc l (snoc xs r) x

init' :: CList a -> CList a
init' EmptyCL = EmptyCL
init' (CUnit a) = CUnit a
init' (Consnoc l xs r) = cons l xs

-- inits que devuelve una lista de CLists - PREGUNTAR
initsAux :: CList a -> [CList a]
initsAux EmptyCL = [EmptyCL]
initsAux lista = EmptyCL : (map (cons (headCL lista)) (initsAux (tailCL lista)))

inits :: CList a -> [CList a]
inits lista = initsAux lista

-- inits' que devuelve una CList de Clists
listToCL :: [a] -> CList a
listToCL []     = EmptyCL
listToCL (x:xs) = cons x (listToCL xs)

inits' :: CList a -> CList (CList a)
inits' lista = listToCL (inits lista) 

{-
    d) Definir una funcion lasts que toma una CList y devuelve una CList con 
    todas las posibles terminaciones de la CList.  
-}
last' :: CList a -> a
last' EmptyCL = error "No rompas las bolas con meter mierdas raras"
last' (CUnit a) = a
last' (Consnoc l xd r) = r

-- lasts :: CList a -> [CList a]
-- lasts (CUnit a) = [EmptyCL, CUnit a]
-- lasts lista = EmptyCL : (map (cons (last' lista)) (lasts (init' lista)))

lasts :: CList a -> [CList a]
lasts lista = map reverseCL (inits (reverseCL lista))
