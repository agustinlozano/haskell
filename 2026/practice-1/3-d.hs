-- d) Int → (Int → Bool) → [Int]

-- By comprehension
myFilter :: Int -> (Int -> Bool) -> [Int]
myFilter n f = [x | x <- [0 .. n], f x]

-- Generic lazy ass fn
fn :: Int -> (Int -> Bool) -> [Int]
fn n f = [n]
