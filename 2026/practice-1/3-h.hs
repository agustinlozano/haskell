-- h) (a, b, c) → Int → c

selectThird :: (a, b, c) -> Int -> c
selectThird (_, _, c) n = c