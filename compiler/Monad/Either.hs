
module Yadorigi.Monad.Either where

instance Monad (Either a) where
    return = Right
    Left a >>= f = Left a
    Right a >>= f = f a
    fail s = error ("Yadorigi.Monad.Either : "++s)

-- choice

(<|>) :: Either e a -> Either e a -> Either e a
Left _ <|> r = r
l <|> _ = l

