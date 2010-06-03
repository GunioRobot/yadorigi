
module Yadorigi.Common
    (amap,iterateToConverge,iterateToConvergeM,iterateToConvergeST,writeLater) where

import Control.Monad.State

amap :: Functor f => a -> f (a -> b) -> f b
amap = fmap.flip id

iterateToConverge :: Eq a => (a -> a) -> a -> a
iterateToConverge f a = let a' = f a in
    if a == a' then a else iterateToConverge f a'

iterateToConvergeM :: (Monad m,Eq a) => (a -> m a) -> a -> m a
iterateToConvergeM f a = do
    a' <- f a
    if a == a' then return a else iterateToConvergeM f a'

iterateToConvergeST :: (MonadState s m,Eq a,Eq s) => (a -> m a) -> a -> m a
iterateToConvergeST f a = do
    s <- get
    a' <- f a
    s' <- get
    if a == a' && s == s' then return a else iterateToConvergeM f a'

writeLater :: a
writeLater = error "Yadorigi.Common.writeLater"

