-- definimos los constructores
data ColaP a = Empty | PQ a (ColaP a)

-- instanciar una nueva cola de prioridad vacia
minimo :: ColaP a
mkqpr = Empty

-- insertar una clave en la cola
insertar' :: (Ord a) => a -> ColaP a -> ColaP a
insertar' x Empty       = (PQ x Empty)
insertar' x (PQ a cola) = PQ a (insertar' x cola)

--buscar el minimo de la cola
siguiente :: (Ord a) => ColaP a -> a
siguiente Empty = error "Empty Queue"
siguiente (PQ x Empty ) = x
siguiente (PQ x (PQ y cola)) | x <= y = siguiente (PQ x cola)
                             | x > y  = siguiente (PQ y cola)

-- extrae el minimo de la cola y devuelve la cola sin ese minimo
extraer :: (Ord a) => ColaP a -> ColaP a
extraer Empty = Empty
extraer (PQ x Empty) = Empty
extraer (PQ x cola)  = if (x == siguiente (PQ x cola)) then cola
                       else (PQ x (extraer cola))