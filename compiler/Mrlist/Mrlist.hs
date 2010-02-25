
module Yadorigi.Mrlist.Mrlist
    ( Mrlist((:!),Mrempty)
    , mrfoldr, mrmap, mrlistTolist
    ) where

-- Data Types

data Mrlist a b = a :! Mrlist b a | Mrempty

-- Fuctions

mrfoldr :: (a -> d -> c) -> (b -> c -> d) -> (c,d) -> Mrlist a b -> c
mrfoldr _ _ (a,b) Mrempty = a
mrfoldr f g (a,b) (x:!xs) = x `f` mrfoldr g f (b,a) xs

mrmap :: (a -> a') -> (b -> b') -> Mrlist a b -> Mrlist a' b'
mrmap f g = mrfoldr ((:!).f) ((:!).g) (Mrempty,Mrempty)

mrlistTolist :: Mrlist a a -> [a]
mrlistTolist = mrfoldr (:) (:) ([],[])
