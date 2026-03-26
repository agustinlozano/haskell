-- 2)
type Linea = (Int, [Char])

vacia :: Linea
vacia = (0, []) 

moverIzq :: Linea -> Linea
moverIzq (p, xs)
    | p /= 0    = (p-1, xs)
    | otherwise = (p, xs)

moverDer :: Linea -> Linea
moverDer (p, xs)
    | p /= length xs = (p+1, xs)
    | otherwise      = (p, xs)

moverIni :: Linea -> Linea
moverIni (p, xs) = (0, xs)

moverFin :: Linea -> Linea
moverFin (p, xs) = (length xs, xs)

insertar :: Char -> Linea -> Linea
insertar c (p, xs) = (p+1, (take p xs) ++ [c] ++ (drop p xs))

borrar :: Linea -> Linea
borrar (0, xs) = (0, xs)
borrar (p, xs) = (p-1, (take (p-1) xs) ++ (drop p xs))