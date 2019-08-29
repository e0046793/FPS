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

sumzero :: [Int] -> Bool
sumzero [] = True
sumzero xs = 0 == foldl (+) 0 xs

c1 :: [Int] -> [[Int]] -> [[Int]]
c1 []     y     = y
c1 (x:xs) [[0]] = [[x]]
c1 (x:xs) value = c1 xs (map (map (+ x)) value)

