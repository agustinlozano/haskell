-- Dado el siguiente tipo algebraico
data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp

{-
    a) Defina un evaluador eval :: Aexp → Int. 
    ¿Como maneja los errores de division por 0?
-}

eval :: Aexp -> Int
eval (Num a)      = a
eval (Prod e1 e2) = eval e1 * eval e2
eval (Div e1 e2)  = eval e1 `div` eval e2

seval :: Aexp -> Maybe Int
seval (Num n)      = Just n
seval (Prod e1 e2) = case seval e1 of
                        Nothing -> Nothing
                        Just n1 -> case seval e2 of
                                    Nothing -> Nothing
                                    Just n2 -> Just (n1 * n2)

seval (Div e1 e2) = case seval e1 of
                        Nothing -> Nothing
                        Just n1 -> case seval e2 of
                                    Nothing -> Nothing
                                    Just n2 -> if n2 == 0
                                                    then Nothing
                                                    else Just (n1 `div` n2)
                                             