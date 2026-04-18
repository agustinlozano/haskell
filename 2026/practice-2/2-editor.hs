type Linea = (Int, [Char])

vacia :: Linea
vacia = (0, [])

moverIzq :: Linea -> Linea
moverIzq (p, chars)
  | p > 0 = (p - 1, chars)
  | otherwise = (p, chars)

moverDer :: Linea -> Linea
moverDer (p, chars)
  | p < length chars = (p + 1, chars)
  | otherwise = (p, chars)

moverIni :: Linea -> Linea
moverIni (p, chars) = (0, chars)

moverFin :: Linea -> Linea
moverFin (p, chars) = (length chars, chars)

insertar :: Char -> Linea -> Linea
insertar c (p, chars) =
  (p + 1, (take p chars) ++ [c] ++ (drop p chars))

borrar :: Linea -> Linea
borrar (p, chars) =
  (p - 1, (take p chars) ++ (drop (p + 1) chars))
