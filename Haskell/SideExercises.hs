--- Find subset of an array
a :: [Int]
a = [6, 3]

nosubset :: [a] -> Int
nosubset xs = 2^(length xs)

subset :: [a] -> [[a]]
subset []     = [[]]
subset (x:xs) = map (x:) (subset xs) ++ subset xs

--- Sum-zero subarrays
-- Given an array, print all subarrays in the array which has sum 0.
b :: [Int]
b = [6, 3, -1, -3, 4, -2, 2, 4, 6, -12, -7]

c :: [Int]
c = [-1, 1, 5, -5, -1]

type Assoc k v = [(k,v)]
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

type HashTable = Assoc Int [Int]

subzerosumset :: [Int] -> [[Int]]
subzerosumset xs = [ v | (k, v) <- subsumset xs, k == 0]

subsumset :: [Int] -> HashTable
subsumset []     = [] 
subsumset (x:xs) = gethash x xs ++ subsumset xs 

nosumset :: [Int] -> Int
nosumset =  length . filter (==0) . sumset 

sumset :: [Int] -> [Int]
sumset []     = []
sumset [n]    = []
sumset (x:xs) = accumualte x xs ++ sumset xs

accumualte :: Int -> [Int] -> [Int]
accumualte _ [] = []
accumualte x ys = [x + sum ys] ++ accumualte x (init ys)

gethash :: Int -> [Int] -> HashTable
gethash _ [] = []
gethash x ys = [((x + sum ys), (x : ys))] ++ gethash x (init ys)

-- Complete the sockMerchant function below.
sockMerchant :: Int -> [Int] -> Int
sockMerchant 0 _      = 0
sockMerchant _ []     = 0
sockMerchant n xs = if n /= length xs then 0 
                        else sum $ map (fst) (hash xs) 

count :: Int -> [Int] -> Int
count x = length . filter (== x)

rmdups :: [Int] -> [Int]
rmdups []     = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)
                          
hash :: [Int] -> [(Int, Int)]
hash [] = []
hash xs = [ (pair $ count color xs, color) | color <- rmdups xs ]

pair :: Int -> Int
pair n = n `div` 2 

-- Repeated String
-- Given an integer, , find and print the number of letter a's in the first  letters of Lilah's infinite string.
repeatedString :: String -> Integer -> Integer
repeatedString s n = toInteger (noains + noainsremain)
                     where 
                        noains        = length $ filter (== 'a') s
                        lstring       = length s
                        evalength     = (fromIntegral n) - lstring
                        nofullsremain = evalength `div` lstring
                        noremainchar  = evalength - nofullsremain * lstring
                        noainsremain  = noains * nofullsremain + (length $ filter (== 'a') $ take noremainchar s)

{- Jumping on the Clouds -}
cl :: [Int] 
cl = [0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0]

jumpingOnClouds :: [Int] -> Int
jumpingOnClouds = length . optimisedjumps 0 . drop 1 . steps 0 . index0bit 

index0bit :: [Int] -> [Int]
index0bit []   = []
index0bit bits = [i | (i,b) <- zip index bits, 0 == b] where index = iterate (+ 1) 0 

steps :: Int -> [Int] -> [Int]
steps _   []   = []
steps n (x:xs) = (x - n) : steps x xs

optimisedjumps :: Int -> [Int] -> [Int]
optimisedjumps _     [] = []
optimisedjumps n (x:xs) = if 1 == n && 1 == x then (n + x) : optimisedjumps (head xs) (drop 1 xs)
                          else n : optimisedjumps x xs




