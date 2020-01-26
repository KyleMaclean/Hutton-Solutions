-- 1

{-
    without the guard against negative arguments, an attempt to compute a negative argument would not terminate because the base case would never be reached
-}
fac :: Int -> Int
fac 1 = 1
fac n  | n > 0 = n * fac (n-1)

-- 2

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- 3

expo :: Num a => a -> Int -> a
m `expo` 0 = 1
m `expo` n = m * m `expo` (n-1)

{-
        2 `expo` 3
    =       { applying `expo` }
        2 * 2 `expo` 2
    =       { applying `expo` }
        2 * 2 * 2 `expo` 1
    =       { applying `expo` }
        2 * 2 * 2 * 2 `expo` 0
    =       { applying `expo` }
        2 * 2 * 2 * 1
    =       { applying * }
        8
-}

-- 4

euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | x < y = euclid x (y-x)
           | otherwise = euclid (x-y) y

-- 5

{-

    length [1,2,3]
    =   { applying length }
    1 + length [2,3]
    =   { applying length }
    1 + 1 + length [3]
    =   { applying length }
    1 + 1 + 1 + length []
    =   { applying length }
    1 + 1 + 1 + 0
    =   { applying + }
    3
    
    drop 3 [1,2,3,4,5]
    =   { applying drop }
    drop 2 [2,3,4,5]
    =   { applying drop }
    drop 1 [3,4,5]
    =   { applying drop }
    drop 0 [4,5]
    =   { applying drop }
    [4,5]
    
    init [1,2,3]
    =   { applying init }
    1 : init [2,3]
    =   { applying init }
    1 : 2 : init 3
    =   { applying init }
    1 : 2 : []
    =   { list notation }
    [1,2]

-}

-- 6

and1 :: [Bool] -> Bool
and1 [] = True
and1 (x:xs) | x == False = False
            | otherwise = and1 xs

concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (x:xs) = x ++ (concat1 xs)

replicate1 :: Int -> a -> [a]
replicate1 0 _ = []
replicate1 n x = [x] ++ replicate1 (n-1) x

bangbang :: [a] -> Int -> a
bangbang (x:xs) 0 = x
bangbang (x:xs) n = bangbang xs (n-1)

elem1 :: Eq a => a -> [a] -> Bool
elem1 _ [] = False
elem1 y (x:xs) | y == x = True
               | otherwise = elem1 y xs

-- 7

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | x > y = y : merge (x:xs) ys

-- 8

halve :: [a] -> ([a],[a])
halve xs = (take mid xs, drop mid xs)
    where mid = (length xs) `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort (fst half)) (msort (snd half))
    where half = halve xs
    
-- 9

sum1 :: Num a => [a] -> a
sum1 [] = 0
sum1 (x:xs) = x + sum1 xs

take1 :: Int -> [a] -> [a]
take1 0 _ = []
take1 n [] = []
take1 n (x:xs) = x : take1 (n-1) xs

last1 :: [a] -> a
last1 [x] = x
last1 (_:xs) = last1 xs
    
    
