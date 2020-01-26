import Data.Char

-- 1

putStr1 :: String -> IO ()
putStr1 xs = sequence_ [putChar x | x <- xs]

-- 2

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard = putBoardRow 1
                    
putBoardRow :: Int -> Board -> IO ()
putBoardRow row [] = return ()
putBoardRow row (x:xs) = do putRow row x
                            putBoardRow (row + 1) xs

-- 3

putBoard1 :: Board -> IO ()
putBoard1 xs = sequence_ [putRow row x | (row,x) <- zip [1..] xs]

-- 4

newline :: IO ()
newline = putChar '\n'

adder :: IO ()
adder = do  putStr "How many numbers? "
            n <- getChar
            newline
            if isDigit n then aux 0 (digitToInt n)
            else
                do  putStrLn "ERROR: Invalid digit"
                    adder

aux :: Int -> Int -> IO ()
aux total 0 = putStr ("The total is " ++ (show total) ++ "\n") 
aux total n = do    x <- getChar
                    if isDigit x then
                        do  newline
                            aux (total + digitToInt x) (n - 1)
                    else
                        do  putStrLn "ERROR: Invalid digit"
                            aux total n

-- 5

--adder1 :: IO ()
--unsolved

-- 6

getLine1 :: IO String
getLine1 = do   x <- getChar
                if x == '\n' then
                    return []
                else
                    do  xs <- getLine1
                        return (x:xs)

bsp :: IO ()
bsp = putStr "\b\b\b"
                        
readLine :: IO String
readLine = do   x <- getChar
                if x == '\n' then
                    return []
                else
                    if x == '\DEL' then
                        do  bsp
                            xs <- readLine
                            return (x:xs)
                    else
                        do  xs <- readLine
                            return (x:xs)
                
