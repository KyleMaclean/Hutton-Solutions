import Data.Char
import Data.List
import System.IO
import System.IO.Unsafe
import System.Random

size :: Int
size = 3

type Grid = [[Player]]

data Player = O | B | X deriving (Eq, Ord, Show)

nextPlayer :: Player -> Player
nextPlayer O = X
nextPlayer B = B
nextPlayer X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/=B) . concat

players :: Grid -> [Player]
players g = concat g

turn :: Grid -> Player
turn g = if os <= xs then O else X
         where
              os = length (filter (== O) (players g))
              xs = length (filter (== X) (players g))

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where
                line = all (== p)
                rows = g
                cols = transpose g
                dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins O g || wins X g

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
          where bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where
               beside = foldr1 (zipWith (++))
               bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && (players g) !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p = if valid g i then [chop size (xs ++ [p] ++ ys)] else []
             where (xs,B:ys) = splitAt i (players g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                      return (read xs)
                   else
                      do putStrLn "ERROR: Invalid number"
                         getNat prompt

tictactoe :: IO ()
tictactoe = run empty O

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int,Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1,1)
             putGrid g
             run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins O g = putStrLn "Player O wins!\n"
         | wins X g = putStrLn "Player X wins!\n"
         | full g = putStrLn "It's a draw!\n"
         | otherwise =
                       do i <- getNat (prompt p)
                          case move g i p of
                               [] -> do putStrLn "ERROR: Invalid move"
                                        run' g p
                               [g'] -> run g' (nextPlayer p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

data Tree a = Node a [Tree a] deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (nextPlayer p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p | won g = []
          | full g = []
          | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 5

minimax :: Tree Grid -> Tree (Grid,Player)
minimax (Node g []) | wins O g = Node (g,O) []
                    | wins X g = Node (g,X) []
                    | otherwise = Node (g,B) []
minimax (Node g ts) | turn g == O = Node (g,minimum ps) ts'
                    | turn g == X = Node (g,maximum ps) ts'
                                    where
                                         ts' = map minimax ts
                                         ps = [p | Node (_,p) _ <- ts']

bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
               where
                    tree = prune depth (gametree g p)
                    Node (_,best) ts = minimax tree
       
main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty O

play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1,1)
              putGrid g
              play' g p

play' :: Grid -> Player -> IO ()
play' g p | wins O g = putStrLn "Player O wins!\n"
          | wins X g = putStrLn "Player X wins!\n"
          | full g = putStrLn "It's a draw!\n"
          | p == O = do i <- getNat (prompt p)
                        case move g i p of
                             [] -> do putStrLn "ERROR: Invalid move"
                                      play' g p
                             [g'] -> play g' (nextPlayer p)
          | p == X = do putStr "Player X is thinking..."
                        (play $! (bestmove'' g p)) (nextPlayer p)

-- 1

-- not sure if this version actually works because stack overflows
nodes :: Tree a -> Int
nodes (Node _ []) = 1
nodes xs = 1 + nodes xs

nodes' :: Tree a -> Int
nodes' (Node _ ts) = 1 + sum (map nodes' ts)

depth' :: Tree a -> Int
depth' (Node _ []) = 0
depth' (Node _ ts) = 1 + maximum (map depth' ts)

-- 2

bestmove' :: Grid -> Player -> Grid
bestmove' g p = bestmoves !! index
                where
                     index = unsafePerformIO (randomRIO (0,(length bestmoves)-1))
                     bestmoves = [g' | Node (g',p') _ <- ts, p' == best]
                     tree = prune depth (gametree g p)
                     Node (_,best) ts = minimax tree

-- 3

bestmove'' :: Grid -> Player -> Grid
bestmove'' g p = bestmoves !! (minIndex depths)
                 where
                      depths = map depth' [Node (g',p') a | Node (g',p') a <- ts, p' == best]
                      bestmoves = [g' | Node (g',p') _ <- ts, p' == best]
                      tree = prune depth (gametree g p)
                      Node (_,best) ts = minimax tree 


minIndex :: [Int] -> Int
minIndex xs = head [y | (x,y) <- (zip xs [0..]), x == minimum xs]

-- 4
-- a (not implemented)
-- b (not implemented)
-- c (not implemented)
-- d (not implemented)

















