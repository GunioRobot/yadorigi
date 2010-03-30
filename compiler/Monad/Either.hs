
module Yadorigi.Monad.Either where

instance Monad (Either a) where
    Left error >>= _ = Left error
    Right a >>= f = f a
    return a = Right a

