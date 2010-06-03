
module Yadorigi.Common (amap,iterateToConverge,iterateToConvergeM,writeLater) where

amap :: Functor f => a -> f (a -> b) -> f b
amap = fmap.flip id

iterateToConverge :: Eq a => (a -> a) -> a -> a
iterateToConverge f a = let a' = f a in
    if a == a' then a else iterateToConverge f a'

iterateToConvergeM :: (Monad m,Eq a) => (a -> m a) -> a -> m a
iterateToConvergeM f a = do
    a' <- f a
    if a == a' then return a else iterateToConvergeM f a'

writeLater :: a
writeLater = error "Yadorigi.Common.writeLater"

