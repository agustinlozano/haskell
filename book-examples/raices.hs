raices :: (Float, Float, Float) -> (Float, Float)
raices (a, b, c)
    | a == 0 = error "No es de segundo grado"
    | e < 0 = error "Raices complejas"
    | otherwise = ((-b + r) / d, (-b - r) / d)
    where r = sqrt e
          d = 2 * a
          e = b * b - 4 * a * c