-- data Maybe a = Nothing | Just a

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)