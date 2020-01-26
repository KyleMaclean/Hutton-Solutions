-- 2

{-
    (2^3)*4
    (2*3)+(4*5)
    2+(3*(4^5))
-}

-- 3

n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]
        
-- 4

last1 :: [x] -> x
last1 xs = head (reverse xs)

last2 :: [x] -> x
last2 xs = xs !! (length xs - 1)

-- 5

init1 :: [x] -> [x]
init1 xs = take (length xs - 1) xs

init2 :: [x] -> [x]
init2 xs = reverse (tail (reverse xs))
