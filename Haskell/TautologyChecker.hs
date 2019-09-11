data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Disj Prop Prop
          | Equi Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

p5 :: Prop
p5 = Disj (p2) (Var 'B')

p6 :: Prop
p6 = Equi p4 p3


type Assoc k v = [(k,v)]
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

type Subst = Assoc Char Bool

-- Evaluate the proposition given a substitution for its variables
eval :: Subst -> Prop -> Bool
eval _   (Const b) = b
eval s     (Var x) = find x s
eval s     (Not p) = not (eval s p)
eval s   (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s  (Disj p q) = eval s p || eval s q
eval s  (Equi p q) = eval s p == eval s q

-- Retrieve all the variables from a proposition
vars :: Prop -> [Char]
vars   (Const _) = []
vars     (Var x) = [x]
vars     (Not p) = vars p
vars   (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars  (Disj p q) = vars p ++ vars q
vars  (Equi p q) = vars p ++ vars q

-- Convert a integer number into a binary number
int2bin :: Int -> [Int]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- remove duplication in an array
rmdups :: Eq a => [a] -> [a]
rmdups     [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)
-- rmdups (x:xs) = x : rmdups (filter (/= x) xs) {-Performance issue-}


-- The key to generating substitutions is producting  lists of logical values of a given length
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n-1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs)) 
           where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]