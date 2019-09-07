{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the compareTriplets function below.
compareTriplets :: [Int] -> [Int] -> [Int]
compareTriplets [] [] = []
compareTriplets xs ys = if [] == scores then [0.0] else scores
                       where
                        results = Data.List.sort $ bits xs ys
                        uniques = rmdups $ results
                        scores = Data.List.reverse $ [Main.count x results | x <- uniques ]

bits :: [Int] -> [Int] -> [Int]
bits     []     [] = []
bits (x:xs) (y:ys) = if x > y then 1 : bits xs ys
                     else if x < y then 0 : bits xs ys
                     else bits xs ys

rmdups :: [Int] -> [Int]
rmdups     [] = []
rmdups (x:xs) = x : Data.List.filter (/= x) (rmdups xs)

count :: Int -> [Int] -> Int
count x = Data.List.length . Data.List.filter (== x)


lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    aTemp <- getLine

    let a = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip aTemp

    bTemp <- getLine

    let b = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip bTemp

    let result = compareTriplets a b

    hPutStrLn fptr $ Data.List.intercalate " " $ Data.List.map (\x -> show x) $ result

    hFlush fptr
    hClose fptr
