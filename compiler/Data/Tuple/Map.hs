
module Yadorigi.Data.Tuple.Map where

import Control.Monad
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

mapM1 :: (Monad m,Sel1 ta a, Upd1 b ta tb) => (a -> m b) -> ta -> m tb
mapM1 f t = liftM (`upd1` t) (f (sel1 t))

mapM2 :: (Monad m,Sel2 ta a, Upd2 b ta tb) => (a -> m b) -> ta -> m tb
mapM2 f t = liftM (`upd2` t) (f (sel2 t))

mapM3 :: (Monad m,Sel3 ta a, Upd3 b ta tb) => (a -> m b) -> ta -> m tb
mapM3 f t = liftM (`upd3` t) (f (sel3 t))

mapM4 :: (Monad m,Sel4 ta a, Upd4 b ta tb) => (a -> m b) -> ta -> m tb
mapM4 f t = liftM (`upd4` t) (f (sel4 t))

mapM5 :: (Monad m,Sel5 ta a, Upd5 b ta tb) => (a -> m b) -> ta -> m tb
mapM5 f t = liftM (`upd5` t) (f (sel5 t))

mapM6 :: (Monad m,Sel6 ta a, Upd6 b ta tb) => (a -> m b) -> ta -> m tb
mapM6 f t = liftM (`upd6` t) (f (sel6 t))

mapM7 :: (Monad m,Sel7 ta a, Upd7 b ta tb) => (a -> m b) -> ta -> m tb
mapM7 f t = liftM (`upd7` t) (f (sel7 t))

mapM8 :: (Monad m,Sel8 ta a, Upd8 b ta tb) => (a -> m b) -> ta -> m tb
mapM8 f t = liftM (`upd8` t) (f (sel8 t))

mapM9 :: (Monad m,Sel9 ta a, Upd9 b ta tb) => (a -> m b) -> ta -> m tb
mapM9 f t = liftM (`upd9` t) (f (sel9 t))

mapM10 :: (Monad m,Sel10 ta a, Upd10 b ta tb) => (a -> m b) -> ta -> m tb
mapM10 f t = liftM (`upd10` t) (f (sel10 t))

mapM11 :: (Monad m,Sel11 ta a, Upd11 b ta tb) => (a -> m b) -> ta -> m tb
mapM11 f t = liftM (`upd11` t) (f (sel11 t))

mapM12 :: (Monad m,Sel12 ta a, Upd12 b ta tb) => (a -> m b) -> ta -> m tb
mapM12 f t = liftM (`upd12` t) (f (sel12 t))

mapM13 :: (Monad m,Sel13 ta a, Upd13 b ta tb) => (a -> m b) -> ta -> m tb
mapM13 f t = liftM (`upd13` t) (f (sel13 t))

mapM14 :: (Monad m,Sel14 ta a, Upd14 b ta tb) => (a -> m b) -> ta -> m tb
mapM14 f t = liftM (`upd14` t) (f (sel14 t))

mapM15 :: (Monad m,Sel15 ta a, Upd15 b ta tb) => (a -> m b) -> ta -> m tb
mapM15 f t = liftM (`upd15` t) (f (sel15 t))

