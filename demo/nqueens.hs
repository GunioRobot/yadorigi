
module Nqueens where

import Data.List

nqueensFilter :: [Int] -> Bool
nqueensFilter [] = True
nqueensFilter (x:xs)
    | or $ zipWith (==) [1..] $ map (abs.subtract x) xs = False
    | otherwise = nqueensFilter xs

nqueens :: Int -> [[Int]]
nqueens n = filter nqueensFilter $ permutations [0..n-1]

nqueensInterface :: Int -> [(Int,Int)] -> [[Int]]
nqueensInterface n cond  = foldr (\(x,y) -> filter $ (y==).(!!x)) (nqueens n) cond

