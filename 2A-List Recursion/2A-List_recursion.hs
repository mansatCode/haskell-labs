

ladies    = ["Mary","Kitty","Lydia","Elizabeth","Jane"]
gentlemen = ["Charles","Fitzwilliam","George","William"]

------------------------- Exercise 1

member :: [String] -> String -> Bool
member    []  _ = False
member (x:xs) y
    | x == y    = True 
    | otherwise = member xs y

member' :: [String] -> String -> Bool
member'    []  _ = False  
member' (x:xs) y = x == y || member' xs y

remove :: [String] -> String -> [String]
remove [] _ = []
remove (x:xs) y
 | x == y = xs
 | otherwise = x : remove xs y


------------------------- Exercise 2

members :: [String] -> [String] -> Bool
members xs    []  = True
members xs (y:ys) 
 | member xs y = members xs ys
 | otherwise   = False

members' :: [String] -> [String] -> Bool
members' xs    []  = True
members' xs (y:ys) = member xs y && members' xs ys 

-- removes every string in ys from the list xs
removeAll :: [String] -> [String] -> [String]
removeAll xs [] = xs
removeAll xs (y:ys) = removeAll (remove xs y) ys

-- removeAll xs [] = xs
-- removeAll xs (y:ys) = remove xs y : removeAll xs ys


------------------------- Exercise 3

before :: [Char] -> [Char] -> Bool
before _ [] = False
before [] _ = False
before (x:xs) (y:ys)
 | x < y = True
 | x > y = False
 | otherwise = before xs ys

before' :: [Char] -> [Char] -> Bool
before' [] _ = False
before' _ [] = False
before' (x:xs) (y:ys) = x < y || x == y && before' xs ys

sorted :: [String] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:xs) = before x (head xs) && sorted xs
