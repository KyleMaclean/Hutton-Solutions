-- 1

halve :: [a] -> ([a],[a])
halve xs = (take (length xs `div` 2) xs,drop (length xs `div` 2) xs)

-- 2

third1 :: [a] -> a
third1 xs = head (tail (tail xs))

third2 :: [a] -> a
third2 xs = xs !! 2

third3 :: [a] -> a
third3 (a:b:c:ds) = c

-- 3

safetail1 :: [a] -> [a]
safetail1 xs = if null xs then [] else tail xs

safetail2 :: [a] -> [a]
safetail2 xs | null xs = []
             | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs

-- 4

disjunction1 :: Bool -> Bool -> Bool
True `disjunction1` True = True
True `disjunction1` False = True
False `disjunction1` True = True
False `disjunction1` False = False

disjunction2 :: Bool -> Bool -> Bool
False `disjunction2` False = False
_ `disjunction2` _ = True

disjunction3 :: Bool -> Bool -> Bool
False `disjunction3` b = b
True `disjunction3` _ = True

disjunction4 :: Bool -> Bool -> Bool
b `disjunction4` c | b == False && c == False = False
                   | otherwise = True
                   
-- 5

conjunction1 :: Bool -> Bool -> Bool
conjunction1 a b =   if a == True then
                        if b == True then
                            True
                        else
                            False
                    else
                        False

-- 6

conjunction2 :: Bool -> Bool -> Bool
conjunction2 a b =  if a == True then b else False

-- 7

mult :: Int -> Int -> Int -> Int
mult = (\x -> (\y -> (\z -> x*y*z)))

-- 8

luhnDouble :: Int -> Int
luhnDouble x | x*2 > 9 = x*2 - 9
             | otherwise = x*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d | (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0 = True
             | otherwise = False
                        
