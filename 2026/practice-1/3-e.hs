-- e) [a] → (a → [b]) → [b]

-- by comprehension
flatMap :: [a] -> (a -> [b]) -> [b]
flatMap xs f = [x | x <- xs, x <- f x]

-- Generic lazy ass version
fn :: [a] -> (a -> [b]) -> [b]
fn (x : xs) f = f x