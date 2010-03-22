
module Yadorigi.Common (amap,iterateToConverge,writeLater) where

import Control.Applicative

amap :: a -> [a -> b] -> [b]
amap = (<**>).pure

iterateToConverge :: Eq a => (a -> a) -> a -> a
iterateToConverge f a = let a' = f a in
    if a == a' then a else iterateToConverge f a'

writeLater :: a
writeLater = error "Yadorigi.Common.writeLater"
