-- i) (a, a, a) → Int → a

retrieveElement :: (a, b, c) -> Int -> a
retrieveElement (a, _, _) n = a
