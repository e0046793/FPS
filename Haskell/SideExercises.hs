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
