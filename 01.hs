-- 1

{-
        double (double 2)
    =       { applying the inner double }
        double (2 + 2)
    =       { applying double }
        (2 + 2) + (2 + 2)
    =       { applying the first + }
        4 + (2 + 2)
    =       { applying the inner + }
        4 + 4
    =       { applying + }
        8
-}

-- 2

{-
        sum [x]
    =       { matching sum }
        sum (x:[])
    =       { applying sum }
        x + sum []
    =       { matching sum }
        x + 0
    =       { applying + considering the identity for addition }
        x
-}

-- 3

product1 :: Num a => [a] -> a
product1 [] = 1
product1 (n:ns) = n * product1 ns

{-
        product1 [2,3,4]
    =       { matching product1 }
        product1 (2:[3,4])
    =       { applying product1 }
        2 * (product1 [3,4])
    =       { matching product1 }
        2 * (product1 (3:4))
    =       { applying product1 }
        2 * 3 * (product1 [4])
    =       { matching product1 }
        2 * 3 * (product1 (4:[]))
    =       { applying product1 }
        2 * 3 * 4 * (product1 [])
    =       { matching product1 }
        2 * 3 * 4 * 1
    =       { applying all * }
        24
-}

-- 4

qsort1 :: Ord a => [a] -> [a]
qsort1 [] = []
qsort1 (x:xs) = qsort1 larger ++ [x] ++ qsort1 smaller
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

{-
        qsort1 [3,5,1,4,2]
    =       { applying qsort1 }
        qsort1 [5,4] ++ [3] ++ qsort1 [1,2]
    =       { applying qsort1 }
        (qsort1 [] ++ [5] ++ qsort1 [4]) ++ [3] ++ (qsort1 [2] ++ [1] ++ qsort1 [])
    =       { applying qsort1 }
        ([] ++ [5] ++ [4]) ++ [3] ++ ([2] ++ [1] ++ [])
    =       { applying all ++ }
        [5,4,3,2,1]
-}

-- 5

qsort2 :: Ord a => [a] -> [a]
qsort2 [] = []
qsort2 (x:xs) = qsort2 smaller ++ [x] ++ qsort2 larger
    where
        smaller = [a | a <- xs, a < x]
        larger = [b | b <- xs, b > x]
        
{-
        qsort2 [2,2,3,1,1]
    =       { applying qsort2 }
        qsort2 [1,1] ++ [2] ++ qsort2 [3]
    =       { applying qsort2 }
        (qsort2 [] ++ [1] ++ qosrt2 []) ++ [2] ++ (qsort2 [] ++ [3] ++ qsort2 [])
    =       { applying qsort2 }
        ([] ++ [1] ++ []) ++ [2] ++ ([] ++ [3] ++ [])
    =       { applying all ++ }
        [1,2,3]
-}
        
        
        
        
        
        
        
        
