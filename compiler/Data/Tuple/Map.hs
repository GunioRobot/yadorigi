
module Yadorigi.Data.Tuple.Map where

import Data.Tuple.All

map1 :: (Sel1 ta a, Upd1 b ta tb) => (a -> b) -> ta -> tb
map1 f t = upd1 (f (sel1 t)) t

map2 :: (Sel2 ta a, Upd2 b ta tb) => (a -> b) -> ta -> tb
map2 f t = upd2 (f (sel2 t)) t

map3 :: (Sel3 ta a, Upd3 b ta tb) => (a -> b) -> ta -> tb
map3 f t = upd3 (f (sel3 t)) t

map4 :: (Sel4 ta a, Upd4 b ta tb) => (a -> b) -> ta -> tb
map4 f t = upd4 (f (sel4 t)) t

map5 :: (Sel5 ta a, Upd5 b ta tb) => (a -> b) -> ta -> tb
map5 f t = upd5 (f (sel5 t)) t

map6 :: (Sel6 ta a, Upd6 b ta tb) => (a -> b) -> ta -> tb
map6 f t = upd6 (f (sel6 t)) t

map7 :: (Sel7 ta a, Upd7 b ta tb) => (a -> b) -> ta -> tb
map7 f t = upd7 (f (sel7 t)) t

map8 :: (Sel8 ta a, Upd8 b ta tb) => (a -> b) -> ta -> tb
map8 f t = upd8 (f (sel8 t)) t

map9 :: (Sel9 ta a, Upd9 b ta tb) => (a -> b) -> ta -> tb
map9 f t = upd9 (f (sel9 t)) t

map10 :: (Sel10 ta a, Upd10 b ta tb) => (a -> b) -> ta -> tb
map10 f t = upd10 (f (sel10 t)) t

map11 :: (Sel11 ta a, Upd11 b ta tb) => (a -> b) -> ta -> tb
map11 f t = upd11 (f (sel11 t)) t

map12 :: (Sel12 ta a, Upd12 b ta tb) => (a -> b) -> ta -> tb
map12 f t = upd12 (f (sel12 t)) t

map13 :: (Sel13 ta a, Upd13 b ta tb) => (a -> b) -> ta -> tb
map13 f t = upd13 (f (sel13 t)) t

map14 :: (Sel14 ta a, Upd14 b ta tb) => (a -> b) -> ta -> tb
map14 f t = upd14 (f (sel14 t)) t

map15 :: (Sel15 ta a, Upd15 b ta tb) => (a -> b) -> ta -> tb
map15 f t = upd15 (f (sel15 t)) t
