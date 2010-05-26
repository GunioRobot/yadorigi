
module Yadorigi.Typing.KindInference where

import Yadorigi.Monad.Either
import Yadorigi.Monad.State
import Yadorigi.Data.Tuple.Map
import Yadorigi.Syntax

import Data.List
import Data.Functor
import Data.Function
import qualified Data.IntMap as IM
import Data.Tuple.All
import Control.Monad.State.Lazy

data KindInferenceError = KindInferenceError deriving Show

type TypeKindEnv = ((ModuleName,String),Kind)
type KindInferenceMonad = StateT (IM.IntMap Kind,[TypeKindEnv],Int) (Either KindInferenceError)

addSubst :: Int -> Kind -> KindInferenceMonad Kind
addSubst n kind@(VarKind m _)
    | m >= n = return kind
addSubst n kind
    | elem n (map fst (getKindvars kind)) = lift $ Left KindInferenceError -- Occurs check
    | otherwise = stateTrans (map1 (IM.insert n kind)) >> return kind

newKindVar :: KindInferenceMonad Int
newKindVar = sel3 <$> fst <$> stateTrans (map3 (1+))

unify :: Kind -> Kind -> KindInferenceMonad Kind
unify AstKind AstKind = return AstKind
unify (FuncKind f a) (FuncKind g b) = liftM2 FuncKind (unify f g) (unify a b)
unify (VarKind n _) kind = addSubst n kind
unify kind (VarKind n _) = addSubst n kind
unify _ _ = lift $ Left KindInferenceError

getKindvars :: Kind -> [(Int,String)]
getKindvars kind = nubBy (on (==) fst) $ getKindvars' kind
    where
        getKindvars' :: Kind -> [(Int,String)]
        getKindvars' AstKind = []
        getKindvars' (FuncKind a b) = getKindvars' a++getKindvars' b
        getKindvars' (VarKind n s) = [(n,s)]
