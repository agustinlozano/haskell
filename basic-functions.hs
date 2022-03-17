not' False = True
not' True = False

-- True and' True = True 
-- _ and' _ = False

head' [] = []
head' (x:_) = x

tail' [] = []
tail' (_:xs) = xs

add' (x, y) = x + y

zeroto' n = [0..n]

currificatedAdd a b = a + b

abs' n | n > 0 = n
       | otherwise = -n

uselessfun n = -1

signum' n | n < 0 = -1
          | n == 0 = 0
          | otherwise = 1
