import Data.Char


halve :: Eq a => [a] -> ([a],[a])
halve x | x == [] = ([],[])
        | False == even (length x) = (x,[])
        | otherwise = splitAt ((length x) `div` 2) x

third :: [a] -> a
{-third [_, _, a] = a -- pattern matching
third (_:xs)    = xs !! 1 -- head and tail
third a         = a !! 2  -- list indexing !!
third xs        = head (tail (tail xs))-}
third (_:_:x:_) = x

safetail :: [a] -> [a]
safetail b | True == null b = [] -- guarded equations
           | otherwise = tail b 
--safetail a      = if True == null a then [] else tail a -- a conditional expression
--safetail []     = []
--safetail (_:xs) = xs -- pattern matching

(&&) :: Bool -> Bool -> Bool
b && c = if b == c then 
            if True == b then True else False
         else False

andFunc :: Bool -> Bool -> Bool
andFunc a b = if a then b else False

mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x * y * z

luhnDouble :: Int -> Int
luhnDouble a = if 9 >= (a * 2) then a * 2 else (a * 2) - 9

luhn :: Int -> Int -> Int -> Int -> Bool
luhn = \x -> \y -> \z -> \w -> 0 == (luhnDouble x + y + luhnDouble z + w) `mod` 10

firsts :: [(a,b)] -> [a]
firsts ps = [x | (x,_) <- ps]

length' :: [a] -> [Int]
length' xs = [1 | _ <- xs]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [1..n], prime x]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

-- positions :: Eq a => a -> [a] -> [Int]
-- positions x xs = [i | (x',i) <- zip xs [0..], x == x']


------ Sum of 1 square to nth square --------
sumSquaresTo :: Int -> Int
sumSquaresTo x = sum [n^2 | n <- [1..x]]


----- 5.7.1 calculated the sum 1^2 + 2^2 +...+ 100^2 -------
sumSquares :: Int
sumSquares = sumSquaresTo 100

----- 5.7.2 Grid  ---------------
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

------- 5.7.3 Square from Grid ---------
--square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

------- 5.7.4 Replicate -------
replicate' :: Int -> a -> [a]
replicate' t v = [v | _ <- [1..t]]

------- 5.7.5 Pythagorean --------
pyths :: Int -> [(Int,Int,Int)]
pyths x = [(a,b,c) | a <- [1..x], b <- [1..x], c <- [1..x], a^2 + b^2 == c^2]

------- 5.7.6 Perfect Integer ---------
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == sum (tail (reverse (factors x)))]

------- 5.7.7 concat application --------------
--comb = [(x,y) | x <- [1,2], y <- [3,4]]
comb = concat [ [ (x,y) | y <- [3,4] ] | x <- [1,2] ]

------- 5.7.8 concat application --------------
positions :: Eq a => a -> [a] -> [Int]
positions k t = find k [(x',i) | (x',i) <- zip t [0..]]

-------- 5.7.9 scalar product --------
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

{- Disable temporary in order to activate Prelude.++
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : xs Main.++ ys
-}

insert :: Ord a => a -> [a] -> [a]
insert x []     = [x]
insert x (y:ys) | x <= y    = x : y : ys
                | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

fibo :: Int -> Int
fibo n | 0 == n = 0
       | 1 == n = 1
       | otherwise = fibo (n - 1) + fibo (n - 2)

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller Prelude.++ [x] Prelude.++ qsort bigger where
                   smaller = [a | a <- xs, a <= x]
                   bigger = [b | b <- xs, b > x] 

even' :: Int -> Bool
even' 0 = True
even' n = odd' (n-1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even' (n-1)

factorial :: Int -> Int
factorial 0 = 1
factorial n = product [1..n]
-- factorial n | 0 > n = n 
--             | otherwise = n * factorial(n-1)

-- 6.8.6.a ----
and' :: [Bool] -> Bool
and' (False:_) = False
and' []        = True
and' (x:xs)    = and' xs

-- 6.8.6.b ----
concat' :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) =  x Prelude.++ concat' xs

-- 6.8.6.c ----
replicate'' :: Int -> a -> [a]
replicate'' 0 _ = []
replicate'' n x = [x] Prelude.++ replicate'' (n-1) x 

-- 6.8.6.d ----
(!!) :: [a] -> Int -> a
(x:xs) !! n | 0 == n = x
            | otherwise = xs Main.!! (n-1)

-- 6.8.6.e ----
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem a (x:xs) | a == x = True
              | otherwise = Main.elem a xs

-- 6.8.7 ----
merge :: Ord a => [a] -> [a] -> [a]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys) | x < y = x : (merge xs (y:ys))
                    | otherwise = merge (y:x:xs) ys -- y : merge (x:xs) ys

-- 6.8.8 --
halve' :: [a] -> ([a],[a])
-- halve' x = (take ((length x) `div` 2) x, drop ((length x) `div` 2) x)
halve' x = splitAt ((length x) `div` 2) x

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort a = merge xs ys where
    xs = msort (fst (halve' a))
    ys = msort (snd (halve' a))

-- 6.8.9.a --
sumr :: Num a => [a] -> a
sumr [] = 0
sumr (x:xs) = x + sumr xs

-- 6.8.9.b --
taker :: Integral a => a -> [b] -> [b]
taker _ []     = []
taker 0 (x:xs) = [x]
taker n (x:xs) = [x] Prelude.++ taker (n-1) xs

-- 6.8.9.c
lastr :: [a] -> a
lastr [a]    = a
lastr (x:xs) = lastr xs 


reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) []

reverse'' :: [a] -> [a]
reverse'' = foldr (\x acc -> x:acc) []


--- 7.9.1
--- map f (filter p xs)

--- 7.9.2.a
-- all' :: (a -> Bool) -> [Bool] -> Bool
-- all' p = and . map p

--- 7.9.2.b
-- any' :: (a -> Bool) -> [Bool] -> Bool
-- any' p = or . map p

--- 7.9.2.c
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f xs = if (snd . head) bs == False then [] 
                  else (fst . head) bs : takeWhile' f (tail xs)
                  where bs = [(x, y) | (x, y) <- zip xs (map f xs)]


--- 7.9.2.d
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f xs = if (snd . head) bs == False then xs
                  else dropWhile' f (tail xs)
                  where bs = [(x, y) | (x, y) <- zip xs (map f xs)] 

--- 7.9.3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x) : acc) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x acc -> if f x then x : acc else acc) []

--- 7.9.4
dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> acc * 10 + x) 0
 
--- 7.9.5
curry :: ((a,b) -> c) -> a -> b -> c
curry f = \x y -> f (x, y)

uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f = \(x, y) -> f x y

--- 7.9.6
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

int2bin :: Int -> [Int]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Int] -> [[Int]]
chop8 = unfold (== []) (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold (null) (f . head) (drop 1) 

iterate'' :: (a -> a) -> a -> [a]
-- iterate'' f = unfold (null . (: [])) (id) (f) 
iterate'' f = unfold (const False) id f

--- 7.9.7
type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x acc -> x + 2 * acc) 0

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id 

encode :: String -> [Bit]
encode = concat . map (addparity . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 = unfold (null) (take 9) (drop 9)

addparity :: [Bit] -> [Bit]
addparity bits = bits ++ [computeparity bits]

computeparity :: [Bit] -> Bit
computeparity bits = sum bits `mod` 2

verify :: [Bit] -> [Bit]
verify (x:xs) = if x == (computeparity xs) then xs else error "bit mismatch"

decode :: [Bit] -> String
decode = map (chr . bin2int . verify) . chop9

--- 7.9.8
faultychannel :: [Bit] -> [Bit]
faultychannel = tail

--- 7.9.9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ []  = [] 
altMap f g xs  = [if Prelude.even i then f v else g v | (i, v) <- zip indices xs] where indices = iterate (+1) 0

--- 7.9.10
luhn' :: [Int] -> Bool
luhn' = Prelude.even . foldr (+) 0 . altMap (id) (luhnDouble)


--- 8.9.1
data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n     = n
add (Succ m) n = Succ (add m n)

multnat :: Nat -> Nat -> Nat
multnat Zero        _ = Zero
multnat (Succ Zero) n = n
multnat m           n = foldl (add) Zero (take (nat2int n) (repeat m))

--- 8.9.2
{-data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = x == y || occurs x r || occurs x l

{- Using compare:: is more efficient than original way since we know which node side to add the value -}
occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y)     = x == y
occurs' x (Node l y r) = if order == EQ then True else
                           if order == LT then occurs' x l else
                            if order == GT then occurs' x r else False
                            where order = compare x y

flatten :: Tree a -> [a]
flatten (Leaf x)   = [x]
flatten (Node l y r) =  flatten l ++ [y] ++ flatten r-}

--- 8.9.3
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

leaves :: Tree a -> Int
leaves (Leaf x) = 1
leaves (Node x y) = leaves x + leaves y

balanced :: Tree a -> Bool
balanced (Leaf x) = True
balanced (Node x y) = leaves x == leaves y

--- 8.9.4
balance :: Ord a => [a] -> Tree a
balance []  = error "empty list"
balance [x] = Leaf x
balance x   = Node (balance lh) (balance rh) 
           where 
            lh = (fst . halve') sorted
            rh = (snd . halve') sorted
            sorted = qsort x