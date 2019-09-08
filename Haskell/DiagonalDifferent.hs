diagonalDifference :: [[Int]] -> Int
diagonalDifference xss = if 0 == halve then 0
                         else abs $ (sumupper halve $ take halve xss) + (sumdowner halve $ reverse $ drop (halve + remain) xss)
                       where 
                        halve = length xss `div` 2 
                        remain = length xss `mod` 2

sumupper :: Int -> [[Int]] -> Int
sumupper _  [] = 0
sumupper 0   _ = 0
sumupper n xss = rightsize - leftside + sumupper (n-1) (init xss)
                 where
                    rightsize = head $ drop (n-1) $ head $ drop (n-1) xss
                    leftside  = head $ drop (n-1) $ reverse $ head $ drop (n-1) xss

sumdowner :: Int -> [[Int]] -> Int
sumdowner _  [] = 0
sumdowner 0   _ = 0
sumdowner n xss = leftside - rightsize + sumdowner (n-1) (init xss)
                 where
                    rightsize = head $ drop (n-1) $ head $ drop (n-1) xss
                    leftside  = head $ drop (n-1) $ reverse $ head $ drop (n-1) xss

diag :: [[Int]] -> Int
diag arr =
    let diagLeft [] p c = c
        diagLeft (x:xs) p c = diagLeft xs (p + 1) (c + x !! p)
        diagRight [] p c = c
        diagRight (x:xs) p c = diagRight xs (p - 1) (c + x !! p)
    in abs((diagLeft arr 0 0) - (diagRight arr (length arr - 1) 0))


t1 :: [[Int]]
t1 = [[1,2,3],[4,5,6],[9,8,9]]

t2 :: [[Int]]
t2 = [[11,2,4],[4,5,6],[10,8,-12]]

t3 :: [[Int]]
t3 = [[6, 6, 7, -10, 9, -3, 8, 9, -1],
      [9, 7, -10, 6, 4, 1, 6, 1, 1],
      [-1, -2, 4, -6, 1, -4, -6, 3, 9],
      [-8, 7, 6, -1, -6, -6, 6, -7, 2],
      [-10, -4, 9, 1, -7, 8, -5, 3, -5],
      [-8, -3, -4, 2, -3, 7, -5, 1, -5],
      [-2, -7, -4, 8, 3, -1, 8, 2, 3],
      [-3, 4, 6, -7, -7, -8, -3, 9, -6],
      [-2, 0, 5, 4, 4, 4, -3, 3, 0]]