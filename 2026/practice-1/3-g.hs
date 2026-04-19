-- g) (a, b, c) → Bool

isSpaceVectorNull :: (Eq a, Eq b, Eq c, Num a, Num b, Num c) => (a, b, c) -> Bool
isSpaceVectorNull (0, 0, 0) = True
isSpaceVectorNull _ = False

-- Mas estricta al enunciado
isOrigin :: (a, b, c) -> Bool
isOrigin (_, _, _) = True
