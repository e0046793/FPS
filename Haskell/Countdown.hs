{- 
Given a sequecne of numbers and a target number, attempt to construct an expression
whose value is the target, by combining one or more numbers from the sequence using addition,
subtraction, multilication, division, and parentheses
-}

s1 :: ([Int],Int)
s1 = ([1,3,7,10,25,50], 765)

data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y


data Expr = Val Int | App Op Expr Expr

-- Display the expression onto standard IO
instance Show Expr where
    show     (Val n) = show n
    show (App o l r) = brak l ++ show o ++ brak r
                       where
                        brak (Val n) = show n
                        brak       e = "(" ++ show e ++ ")"

-- Get all the integer values from an expression
values :: Expr -> [Int]
values     (Val n) = [n]
values (App o l r) = values l ++ values r

-- Returns the overall value of an expression
eval :: Expr -> [Int]
eval     (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

-- Returns all subsequences of a list, 
subs :: [a] -> [[a]]
subs     [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

-- Return all possible way of inserting a new element into a list
interleave :: a -> [a] -> [[a]]
interleave x     [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- Return all permutations of a list
-- i.e. The possible permutations of x, y and z are xyz, xzy, yxz, yzx, zxy and zyx.
perms :: [a] -> [[a]]
perms    []  = [[]]
perms (x:xs) =  concat $ map (interleave x) (perms xs)

-- Return all choices from a list, which are given by all possible ways of selecting zero or more selements in any order
choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- An expression is a solution for a given list of numbers and a targer if
-- the list of values in the expression is chosen from the list of numbers
-- AND the expression successfullly evaluates to give the targt
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

e :: Expr
e = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))