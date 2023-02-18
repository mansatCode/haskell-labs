

ladies    = ["Mary","Kitty","Lydia","Elizabeth","Jane"]
gentlemen = ["Charles","Fitzwilliam","George","William"]
couples   = [("Elizabeth","Fitzwilliam"),("Charlotte","William"),("Lydia","George"),("Jane","Charles")]

------------------------- Exercise 1


ditch :: Int -> [a] -> [a]
ditch x (y:ys)
 | x == 0 = (y:ys)
 | otherwise = ditch (x-1) ys

at :: [a] -> Int -> a
at (x:xs) i 
 | i < 0 = error "Index negative"
 | i >= length (x:xs) = error "Index too large"
 | i == 0 = x
 | otherwise = at xs (i-1) 


------------------------- Exercise 2

-- Complete the function find which given an item of type a , 
-- looks up the related item of b in a list of pairs
find :: Eq a => a -> [(a,b)] -> b
find a [] = error "Not found"
find a ((x,y):xs)
 | a == x = y
 | otherwise = find a xs

which :: Eq a => a -> [a] -> Int
which = aux 0
  where
    aux :: Eq a => Int -> a -> [a] -> Int
    aux n a [] = error "Not found"
    aux n a (x:xs)
     | a == x = n
     | otherwise = aux (n+1) a xs

-- Copy member, remove and before from 2A and change
-- their type so that they work for as many lists as possible

-- member :: [String] -> String -> Bool
-- member    []  _ = False
-- member (x:xs) y
--     | x == y    = True 
--     | otherwise = member xs y

member :: Eq a => [a] -> a -> Bool
member    []  _ = False
member (x:xs) y
    | x == y    = True 
    | otherwise = member xs y

-- remove :: [String] -> String -> [String]
-- remove [] _ = []
-- remove (x:xs) y
--  | x == y = xs
--  | otherwise = x : remove xs y

remove :: Eq a => [a] -> a -> [a]
remove [] _ = []
remove (x:xs) y
 | x == y = xs
 | otherwise = x : remove xs y

-- before :: [Char] -> [Char] -> Bool
-- before _ [] = False
-- before [] _ = False
-- before (x:xs) (y:ys)
--  | x < y = True
--  | x > y = False
--  | otherwise = before xs ys

before :: Ord a => [a] -> [a] -> Bool
before _ [] = False
before [] _ = False
before (x:xs) (y:ys)
 | x < y = True
 | x > y = False
 | otherwise = before xs ys

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [x] = True
-- sorted (x:xs) = before x (head xs) && sorted xs
sorted (x:y:xs) = x <= y && sorted (y:xs)

------------------------- Exercise 3

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
 | x < y = x : merge xs (y:ys)
 | y < x = y : merge (x:xs) ys
 | x == y = x : merge xs ys


minus :: Ord a => [a] -> [a] -> [a]
minus xs [] = xs
minus [] ys = []
minus (x:xs) (y:ys) 
 | x == y = minus xs ys
 | otherwise = x : minus xs (y:ys)


msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort (x:xs) = (msort (take (length (x:xs) `div` 2) (x:xs))) `merge` (msort (drop (length (x:xs) `div` 2) (x:xs)))
               