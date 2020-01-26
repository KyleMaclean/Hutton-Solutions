import Data.Char

-- 1

func f p xs = map f (filter p xs)

-- 2

all1 :: (a -> Bool) -> [a] -> Bool
all1 p = and . map p
              
any1 :: (a -> Bool) -> [a] -> Bool             
any1 p = or . map p

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 _ [] = []
takeWhile1 p (x:xs) | p x = x : takeWhile1 p xs
                    | otherwise = []
                    
dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 _ [] = []
dropWhile1 p (x:xs) | p x = dropWhile1 p xs
                    | otherwise = x:xs

-- 3

map1 :: (a -> b) -> [a] -> [b]
map1 f [] = []
map1 f (x:xs) = f x : map1 f xs

map2 :: (a -> b) -> [a] -> [b]
map2 f = foldr (\x xs -> f x : xs) []

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p [] = []
filter1 p (x:xs) | p x = x : filter1 p xs
                 | otherwise = filter1 p xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p = foldr (\x xs -> if p x then x:xs else xs) []

-- 4

dec2int1 :: [Int] -> Int
dec2int1 [] = 0
dec2int1 (x:xs) = x * (10 ^ length xs) + dec2int1 xs

dec2int2 :: [Int] -> Int
dec2int2 = foldl (\x xs -> x * 10 + xs) 0

-- 5

curry1 :: ((a,b) -> c) -> (a -> b -> c)
curry1 f x y = f (x,y)

curry2 :: ((a,b) -> c) -> (a -> b -> c)
curry2 f = \x y -> f (x,y)
    
uncurry1 :: (a -> b -> c) -> ((a,b) -> c)
uncurry1 f (x,y) = f x y

uncurry2 :: (a -> b -> c) -> ((a,b) -> c)
uncurry2 f = \(x,y) -> f x y

-- 6

unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

map3 :: (a -> b) -> [a] -> [b]
map3 f = unfold null (f . head) tail

iterate1 :: (a -> a) -> a -> [a]
iterate1 f = unfold (\x -> False) id f

-- 7

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

parity :: [Bit] -> [Bit]
parity bits | ones `mod` 2 == 1 = bits ++ [1]
            | otherwise = bits ++ [0]
    where ones = length (filter (==1) bits)

encode :: String -> [Bit]
encode = concat . map (parity . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

verify :: [Bit] -> Bool
verify bits | ones `mod` 2 == last bits = True
            | otherwise = False
    where ones = length (filter (==1) (init bits))

decode :: [Bit] -> String
decode bits | all verify (chop9 bits) = map (chr . bin2int . init) (chop9 bits)
            | otherwise = error "data corruption"

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- 8

faultychannel :: [Bit] -> [Bit]
faultychannel = tail

badtransmit :: String -> String
badtransmit = decode . faultychannel . encode

-- 9

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g [x] = f x : altMap f g []
altMap f g (x1:x2:xs) = f x1 : g x2 : altMap f g xs

-- 10

luhnDouble :: Int -> Int
luhnDouble x | x*2 > 9 = x*2 - 9
             | otherwise = x*2

luhn :: [Int] -> Bool
luhn xs | sum (altMap luhnDouble id xs) `mod` 10 == 0 = True
        | otherwise = False

