{-
    Generate solutions using [1,2,3] to equal 9:
    >   solutions [1,2,3] 9
    
    Order solutions by increasing number of brackets:
    >   sortstring (solutions [1,2,3] 9)
    
-}

import Data.List

data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Exp = "^"
    
valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 0 && y /= 1 && x `mod` y == 0
valid Exp x y = y > 1 && x > 1

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n) = show n
    show (App o l r) = brak l ++ show o ++ brak r
        where
            brak (Val n) = show n
            brak e = "(" ++ show e ++ ")"
            
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

evals :: [Expr] -> [Int]
evals [] = []
evals (e:es) = eval e ++ evals es

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
    where yss = subs xs
          
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

choices' :: [a] -> [[a]]
choices' xs = [css | ss <- subs xs, css <- perms ss]

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x:xs) [] = False
isChoice (x:xs) ys 
    | success = isChoice xs removed
    | otherwise = False
    where
        success = length ys > length removed
        removed = (remfirst x ys)
        
isChoice' :: Eq a => [a] -> [a] -> Bool
isChoice' [] _ = True
isChoice' (x:xs) [] = False
isChoice' (x:xs) ys = elem x ys && isChoice' xs (remfirst x ys)
                   

remfirst :: Eq a => a -> [a] -> [a]
remfirst n [] = []
remfirst n (x:xs) | n == x = xs
                  | otherwise = x : remfirst n xs

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

choicesexprs :: [Int] -> [Expr]
choicesexprs ns = [e | ns' <- choices ns, e <- exprs ns']

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add,Sub,Mul,Div,Exp]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

type Result = (Expr,Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n) | n > 0]
results ns = [res | (ls,rs) <- split ns, lx <- results ls, ry <- results rs, res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]

nearest :: [Int] -> Int -> Int -> [Expr]
nearest ns n c | not (null highertarget) = highertarget
               | not (null lowertarget) = lowertarget
               | otherwise = nearest ns n (c+1)
               where
                   highertarget = solutions' ns (n+c)
                   lowertarget = solutions' ns (n-c)

exprstring :: [Expr] -> [String]
exprstring [] = []
exprstring (e:es) = [show e] ++ exprstring es

sortstring :: [Expr] -> [String]
sortstring e = sortOn (length . filter (=='(')) (exprstring e)
