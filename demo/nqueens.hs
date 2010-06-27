
module Nqueens where

import Data.List
import Control.Applicative
import Control.Monad

nqueens :: Int -> [[Int]]
nqueens n = filter (all f.tails) $ permutations [0..n-1] where
    f [] = True
    f (x:xs) = and $ zipWith ((/=).abs.subtract x) xs [1..]

nqueensInterface :: Int -> [(Int,Int)] -> [[Int]]
nqueensInterface n cond  = foldr (\(x,y) -> filter $ (y==).(!!x)) (nqueens n) cond

main :: IO ()
main = liftM2 nqueensInterface readLn readLn >>= print

