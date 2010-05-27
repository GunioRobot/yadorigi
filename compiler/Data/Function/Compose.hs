
module Yadorigi.Data.Function.Compose where

import Control.Applicative

oo :: (c -> d) -> (a -> b -> c) -> a -> b -> d
oo = (.).(.)

ooo , (...) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
ooo = oo.(.)
(...) = ooo

oooo , (....) :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
oooo = ooo.(.)
(....) = oooo

ooooo , (.....) :: (f -> g) -> (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> g
ooooo = oooo.(.)
(.....) = ooooo

oooooo , (......) ::
    (g -> h) ->
    (a -> b -> c -> d -> e -> f -> g) ->
    a -> b -> c -> d -> e -> f -> h
oooooo = ooooo.(.)
(......) = oooooo

ooooooo , (.......) ::
    (h -> i) ->
    (a -> b -> c -> d -> e -> f -> g -> h) ->
    a -> b -> c -> d -> e -> f -> g -> i
ooooooo = oooooo.(.)
(.......) = ooooooo

oooooooo , (........) ::
    (i -> j) ->
    (a -> b -> c -> d -> e -> f -> g -> h -> i) ->
    a -> b -> c -> d -> e -> f -> g -> h -> j
oooooooo = ooooooo.(.)
(........) = oooooooo

ooooooooo , (.........) ::
    (j -> k) ->
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) ->
    a -> b -> c -> d -> e -> f -> g -> h -> i -> k
ooooooooo = oooooooo.(.)
(.........) = ooooooooo

oooooooooo , (..........) ::
    (k -> l) ->
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) ->
    a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> l
oooooooooo = ooooooooo.(.)
(..........) = oooooooooo

(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
(<.>) = (.).(<$>)

(<..>) :: Functor f => (c -> d) -> (a -> b -> f c) -> a -> b -> f d
(<..>) = oo.(<$>)

(<...>) :: Functor f => (d -> e) -> (a -> b -> c -> f d) -> a -> b -> c -> f e
(<...>) = ooo.(<$>)

