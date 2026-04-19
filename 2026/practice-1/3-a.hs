-- Ignores first fn and returns not fn
negateBoolFunc :: (Int -> Int) -> (Bool -> Bool)
negateBoolFunc _ = not

-- Ignores first fn and return a lamda
transformFunc :: (Int -> Int) -> (Bool -> Bool)
transformFunc _ = \b -> b

-- Generic lazy ass fn
fn :: (Int -> Int) -> (Bool -> Bool)
fn _ b = b

-- 🚫 🚩 This is a bad approach to tackling/solve these exercises
-- Much easier generic functions
randomShit :: (Int -> Int) -> (Bool -> Bool)
randomShit f = if f 0 > 10 then not else id