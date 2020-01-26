-- 1

data Nat = Zero | Succ Nat deriving Show

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult (Succ (Zero)) n = n
mult (Succ m) n = mult (add m m) n

-- 2

data Tree1 a = Leaf1 a | Node1 (Tree1 a) a (Tree1 a)

occurs :: Ord a => a -> Tree1 a -> Bool
occurs x (Leaf1 y) = x == y
occurs x (Node1 l y r) | x == y = True
                       | x < y = occurs x l
                       | x > y = occurs x r

{-
    This definition of occurs is more efficient for search trees specifically but is incomplete for trees in general. It ignores branches in which the query is guaranteed to not be found due to the search tree being ordered.
-}

-- 3

data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a) deriving Show
t :: Tree2 Int
t = Node2 (Node2 (Node2 (Leaf2 3) (Leaf2 4)) (Leaf2 5)) (Leaf2 3)

leaves :: Tree2 a -> Int
leaves (Leaf2 x) = 1
leaves (Node2 x y) = 0 + leaves x + leaves y

balanced :: Tree2 a -> Bool
balanced (Leaf2 x) = True
balanced (Node2 x y) | diff < 2 && diff > (-2) = balanced x && balanced y
                     | otherwise = False
    where diff = leaves x - leaves y

-- 4

halve :: [a] -> ([a],[a])
halve xs = (take len xs, drop len xs)
    where len = (length xs) `div` 2

balance :: [a] -> Tree2 a
balance [x] = Leaf2 x
balance xs = Node2 (balance x) (balance y)
    where (x,y) = halve xs

-- 5

data Expr1 = Val1 Int | Add1 Expr1 Expr1

folde1 :: (Int -> a) -> (a -> a -> a) -> Expr1 -> a
folde1 f g (Val1 x) = f x
folde1 f g (Add1 y z) = g (folde1 f g y) (folde1 f g z)

-- 6

eval1 :: Expr1 -> Int
eval1 = folde1 id (+)

size :: Expr1 -> Int
size (Val1 x) = 1
size (Add1 y z) = size y + size z

-- 7

data Maybe1 a = Nothing1 | Just1 a deriving (Show, Ord)

instance Eq a => Eq (Maybe1 a) where
    Nothing1 == Nothing1 = True
    (Just1 x) == (Just1 y) = x == y
    _ == _ = False

-- unsolved:
-- instance Eq a => Eq [a] where ...

-- 8

type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          | Equiv Prop Prop
          
type Subst = Assoc Char Bool

eval2 :: Subst -> Prop -> Bool
eval2 _ (Const b) = b
eval2 s (Var x) = find x s
eval2 s (Not p) = not (eval2 s p)
eval2 s (And p q) = eval2 s p && eval2 s q
eval2 s (Imply p q) = eval2 s p <= eval2 s q
eval2 s (Or p q) = eval2 s p || eval2 s q
eval2 s (Equiv p q) = eval2 s p == eval2 s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
    where bss = bools (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)
    
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
    where vs = rmdups (vars p)
          
isTaut :: Prop -> Bool
isTaut p = and [eval2 s p | s <- substs p]

-- 9

data Expr2 = Val2 Integer | Add2 Expr2 Expr2 | Mul2 Expr2 Expr2

folde2 :: Num a => (a -> a -> a) -> (a -> a -> a) -> Expr2 -> a
folde2 f g (Val2 x) = fromInteger x
folde2 f g (Add2 y z) = f (folde2 f g y) (folde2 f g z)
folde2 f g (Mul2 y z) = g (folde2 f g y) (folde2 f g z)

eval3 :: Expr2 -> Int
eval3 = folde2 (+) (*)
