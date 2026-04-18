data Color = RGB Int Int Int
  deriving (Show)

mezclar :: Color -> Color -> Color
mezclar (RGB r1 v1 a1) (RGB r2 v2 a2) = RGB (div (r1 + r2) 2) (div (v1 + v2) 2) (div (a1 + a2) 2)
