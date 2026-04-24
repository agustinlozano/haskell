{-
    4. Dado el siguiente tipo algebraico:
    data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp

    a) Defina un evaluador eval :: Aexp → Int. ¿Como maneja los errores de division por 0?
    b) Defina un evaluador seval :: Aexp → Maybe Int.
-}

data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp deriving (Show)

-- A)
eval :: Aexp -> Int
eval (Num x) = x
eval (Prod x y) = eval x * eval y
eval (Div x y)
  | denom == 0 = error "div by zero"
  | otherwise = eval x `div` denom
  where
    denom = eval y

-- B)
seval :: Aexp -> Maybe Int
seval (Num x) = Just x
seval (Prod x y) =
  case seval x of
    Nothing -> Nothing
    Just vx ->
      case seval y of
        Nothing -> Nothing
        Just vy -> Just (vx * vy)
seval (Div x y) =
  case seval x of
    Nothing -> Nothing
    Just vx ->
      case seval y of
        Nothing -> Nothing
        Just vy ->
          if vy == 0
            then Nothing
            else Just (vx `div` vy)
