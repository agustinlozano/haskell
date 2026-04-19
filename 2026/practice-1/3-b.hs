-- b) Bool → (Int → Bool)

something c = if c then even else odd

-- Usando pattern matching
somethingPm :: Bool -> (Int -> Bool)
somethingPm True = even
somethingPm False = odd

-- Generic lazy ass fn
fn :: Bool -> (Int -> Bool)
fn p = (\x -> p)