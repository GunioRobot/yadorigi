
module Yadorigi.Monad.Either where

instance Monad (Either a) where
    return a = Right a
    Left error >>= _ = Left error
    Right a >>= f = f a
    fail s = error ("Yadorigi.Monad.Either : "++s)

