data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving (Show)

-- `EmptyCL` Empty list
-- `CUnit a` List with a single element
-- `Consnoc c | (CList a) a` List with one element at the front (a), a recursive middle, and one element at the end (a)

{-
    A) Implement the operations for this algebraic type:
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
isEmptyCl _ = False -- The underscore catches CUnit and Consnoc

isCUnit :: CList a -> Bool
isCUnit (CUnit _) = True
isCUnit _ = False

tailCl :: CList a -> CList a
tailCl EmptyCL = EmptyCL
tailCl (CUnit a) = EmptyCL
tailCl (Consnoc l xs r)
  | isEmptyCl xs = CUnit r
  | otherwise = Consnoc (headCl xs) (tailCl xs) r

-- AI-optimized version
tailCl' :: CList a -> CList a
tailCl' (Consnoc l EmptyCL r) = CUnit r
tailCl' (Consnoc l (CUnit m) r) = Consnoc m EmptyCL r
tailCl' (Consnoc l (Consnoc m ys n) r) = Consnoc m (tailCl (Consnoc m ys n)) r

{-
    B) Define a reverseCL function that takes a CList and returns its reverse
-}
-- My version
reverseCl :: CList a -> CList a
reverseCl (Consnoc l EmptyCL r) = Consnoc r EmptyCL l
reverseCl (Consnoc l (CUnit m) r) = Consnoc r (CUnit m) l
reverseCl (Consnoc l (Consnoc m ys n) r) = Consnoc r (reverseCl (Consnoc m ys n)) l

-- Simplified by AI
reverseCl' :: CList a -> CList a
reverseCl' EmptyCL = EmptyCL
reverseCl' (CUnit x) = CUnit x
reverseCl' (Consnoc l xs r) = Consnoc r (reverseCl' xs) l

{-
    C) Define an inits function that takes a CList and
    returns a CList with all possible prefixes of the CList.
-}

-- helper to take an element and append it to the end of the CList
snocCl :: a -> CList a -> CList a
snocCl x EmptyCL = CUnit x
snocCl x (CUnit y) = Consnoc y EmptyCL x
snocCl x (Consnoc l xs r) = Consnoc l (snocCl r xs) x

inits :: CList a -> CList (CList a)
inits EmptyCL = CUnit EmptyCL
inits (CUnit a) = Consnoc EmptyCL EmptyCL (CUnit a)
inits cl = snocCl cl (inits (initCl cl))
  where
    -- initCl returns the CList without the last element
    initCl (CUnit _) = EmptyCL
    initCl (Consnoc l xs r)
      | isEmptyCl xs = CUnit l
      | otherwise = Consnoc l (initCl (snocCl r xs)) (headCl (reverseCl (snocCl r xs)))

-- Note: initCl is the mirror of the tailCL we made earlier.