--------- Definicion de nuevo tipo
data Persona = Pers {nombre::String, dni::Int, edad::Int} deriving Show

instance Eq Persona where
 (==) p1 p2 = dni p1 == dni p2
 (/=) p1 p2 = not (p1 == p2)

-- Cargar datos
arturo = Pers {nombre="Arturo", dni=28053696, edad=31}
zoilo = Pers {nombre="Zoilo", dni=21654987, edad=35}
luis = Pers {nombre ="Luis", dni=22654987, edad=33}
