-- f) [[a]] → (a → Bool) → [a]

-- by comprehension
flat :: [[a]] -> (a -> Bool) -> [a]
flat xss f = [x | xs <- xss, x <- xs]

-- Generic lazy ass version
fn :: [[a]] -> (a -> Bool) -> [a]
fn (x : xss) f = x