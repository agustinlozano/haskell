{-
  8) Un CONJUNTO o SET, es una colección de ítems distintos, 
  en la cual un ítem puede ser: 
  a)testeado si es miembro, b)insertado o, c)borrado de la colección.
  La cantidad  de elementos distintos es lo que se denomina el tamaño 
  del conjunto.

  Module Set (Set,  emptySet, setEmpty, inSet, addSet, delSet) where
    emptySet  ::  Set a 

    setEmpty  ::  Set a  -> Bool

    inSet     :: (Eq a) => a -> Set a -> Bool

    addSet    :: (Eq a) => a -> Set a -> Set a

    delSet    :: (Eq a) => a -> Set a -> Set a

    unionSet  :: (Eq a) => Set a -> Set a -> Set a
-}

-- definicion de tipo
module Set(Set, emptySet, setEmpty, addSet, inSet, delSet, unionSet) where

newtype Set a = St [a] deriving Show

-- metodos del tipo de dato
emptySet :: Set a
emptySet = St []

setEmpty :: Set a -> Bool
setEmpty (St []) = True
setEmpty (St _) = False

inSet :: (Eq a) => a -> Set a -> Bool
inSet x (St []) = False
inSet x (St (y:ys)) = x == y || inSet x (St (ys))

addSet :: (Eq a) => a-> Set a -> Set a
addSet x (St xs) = if inSet x (St xs) then 
                      St (xs) 
                   else St (x:xs)

delSet :: (Eq a) => a -> Set a -> Set a
delSet x (St []) = St []
delSet x (St (y:ys)) = if x /= y then 
                          addSet y (delSet x (St (ys))) 
                       else delSet x (St (ys))

unionSet :: (Eq a) => Set a -> Set a -> Set a
unionSet (St []) (St (ys)) = St (ys)
unionSet (St (x:xs)) (St (ys)) = if inSet x (St (ys)) then 
                                    unionSet (St (xs)) (St (ys)) 
                                  else unionSet (St (xs)) (St (x:ys))
