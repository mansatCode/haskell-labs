------------------------- Exercise 1
doubles :: [Int] -> [Int]
doubles [] = []
doubles (x:xs) = (x*2) : doubles xs

-- doubles :: [Int] -> [Int]
-- doubles (x:xs) = map (\x -> x*2) (x:xs)

odds :: [Int] -> [Int]
odds [] = []
odds (x:xs) = if odd x then x : odds xs else odds xs

-- odds :: [Int] -> [Int]
-- odds [] = []
-- odds (x:xs) = filter odd (x:xs)

doubleodds :: [Int] -> [Int]
doubleodds [] = []
doubleodds (x:xs) = if odd x then (x*2) : doubleodds xs else doubleodds xs 

-- doubleOdds :: [Int] -> [Int]
-- doubleOdds [] = []
-- doubleOdds (x:xs) = map (\x -> x*2) (filter odd (x:xs))


------------------------- Exercise 2

shorts :: [String] -> [String]
shorts [] = []
shorts (x:xs) = filter checkLen (x:xs) 
    where
        checkLen :: String -> Bool
        checkLen a = if (length a) <= 5 then True else False

squarePositives :: [Int] -> [Int]
squarePositives [] = []
squarePositives (x:xs) = map square (filter isPositive (x:xs))
    where   
        square :: Int -> Int
        square x = x*x

        isPositive :: Int -> Bool
        isPositive x = if x > 0 then True else False

oddLengthSums :: [[Int]] -> [Int]
oddLengthSums [] = []
oddLengthSums (x:xs) = map sumOf (filter oddLenList (x:xs))
    where
        oddLenList :: [Int] -> Bool
        oddLenList (x:xs) = if odd (length (x:xs)) then True else False

        sumOf :: [Int] -> Int
        sumOf (x:xs) = sum (x:xs)


------------------------- Exercise 3

remove :: Eq a => [a] -> a -> [a]
remove = undefined

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll = undefined

numbered :: [a] -> [(Int,a)]
numbered (x:xs) = zip [1..(length (x:xs))] (x:xs)

everyother :: [a] -> [a]
everyother (x:xs) = map snd ( filter selectOdd (numbered (x:xs)) )
    where
        selectOdd :: (Int,a) -> Bool
        selectOdd x = odd (fst x)

same :: Eq a => [a] -> [a] -> [Int]
same (x:xs) (y:ys) = map fst (map fst (filter eqs (zip (numbered (x:xs)) (y:ys))))
    where
        eqs :: Eq a => ((Int,a),a) -> Bool
        eqs x = if ( (snd (fst x)) == (snd x) ) then True else False
