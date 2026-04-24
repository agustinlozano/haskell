{-
    Helper `contains`
  Given a list and an element, checks whether
  the element is contained in the list
-}

contains :: (Eq a) => a -> [a] -> Bool
contains _ [] = False
contains n (x : xs)
  | n == x = True
  | otherwise = contains n xs

{-
    Helper `uniques`
  Given a list, returns another with unique values (without duplicates)
-}

uniques :: (Eq a) => [a] -> [a]
uniques xs = f xs []
  where
    f [] acc = acc
    f (x : xs) acc
      | contains x acc = f xs acc
      | otherwise = f xs (x : acc)

{-
    Helper `frequency`
  Given an element and a list, returns how many times
  the element appears in the list
-}

frequency :: (Eq a) => a -> [a] -> Int
frequency e [] = 0
frequency e xs = f xs 0
  where
    f [] acc = acc
    f (x : xs) acc
      | x == e = f xs (acc + 1)
      | otherwise = f xs acc

{-
    Helper `frequencies`
  Given a list, maps to another where each element is a tuple (v,f)
  Where:
    v is a unique value from the original list
    f is the frequency of that value in it
-}

frequencies :: (Eq a) => [a] -> [(a, Int)]
frequencies [] = []
frequencies xs = map (\x -> (x, frequency x xs)) (uniques xs)

{-
    d) samePairs :: Int → Int → Int → Int → Bool

  takes 4 integers and returns True if they are equal in pairs (in any order),
  in all other cases it returns False. For example:

    samePairs 3 1 1 2 = False
    samePairs 3 1 3 1 = True
    samePairs 3 3 1 1 = True
    samePairs 3 1 1 3 = True
-}

{-
  My solution
  Convert a combinations problem into a frequencies problem

  To do this:
  - Treat the values as a list
  - Find the unique values in the original list
  - Find the frequency with which each unique value appears in the original list
  - Finally, verify that the exercise conditions are satisfied
-}

samePairs :: (Eq a) => a -> a -> a -> a -> Bool
samePairs a b c d = (length freqs == 2) && all validFreq freqs
  where
    xs = [a, b, c, d]
    freqs = frequencies xs
    validFreq (_, f) = f == 2