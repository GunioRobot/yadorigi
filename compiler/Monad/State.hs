
module Yadorigi.Monad.State where

import Control.Monad.State

stateTrans :: MonadState s m => (s -> s) -> m (s,s)
stateTrans f = do
    s <- get
    let s' = f s
    put s'
    return (s,s')

