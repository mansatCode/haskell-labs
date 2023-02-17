

------------------------- Exercise 1

square :: Int -> Int
square x = x * x

pythagoras a b c = square(a) + square(b) == square(c)


------------------------- Exercise 2

factorial :: Int -> Int
factorial x
    | x <= 1    = 1
    | otherwise = factorial(x-1) * x

euclid :: Int -> Int -> Int
euclid x y
    | x <= 0 || y <= 0 = error "euclid: negative argument"
    | x == y = x
    | x <  y = euclid x (y-x)
    | x >  y = euclid y (x-y)

power :: Int -> Int -> Int
power a b
 | b == 0 = error "power: negative argument"
 | b == 1 = a
 | b > 1 = a * power a (b-1)


------------------------- Exercise 3

range :: Int -> Int -> [Int]
range x y
 | x == y = [x]
 | x < y = x : range (x+1) y
 | otherwise = []

times :: [Int] -> Int
times x
 | x == [] = 1
 | otherwise = head x * times (tail x)
--note: you will need to create your own pattern-matching

fact :: Int -> Int
fact x = times (range 1 x)
