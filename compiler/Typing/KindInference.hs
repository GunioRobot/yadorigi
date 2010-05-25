
module Yadorigi.Typing.KindInference where

import Yadorigi.Monad.Either
import Yadorigi.Monad.State
import Yadorigi.Data.Tuple.Map
import Yadorigi.Syntax

import Data.Functor
import qualified Data.IntMap as IM
import Data.Tuple.All
import Control.Monad.State.Lazy

data KindInferenceError = KindInferenceError deriving Show

type TypeKindEnv = ((ModuleName,String),Kind)
type KindInferenceMonad =
    StateT (IM.IntMap Kind,[TypeKindEnv],Int) (Either KindInferenceError)

addSubst :: Int -> Kind -> KindInferenceMonad Kind
addSubst n kind = stateTrans (map1 (IM.insert n kind)) >> return kind

newKindVar :: KindInferenceMonad Int
newKindVar = sel3 <$> fst <$> stateTrans (map3 (+1))

unify :: Kind -> Kind -> KindInferenceMonad Kind
unify AstKind AstKind = return AstKind
unify (FuncKind f a) (FuncKind g b) = liftM2 FuncKind (unify f g) (unify a b)
unify (VarKind n s) (VarKind n' s')
    | n == n' = return $ VarKind n s
    | n > n' = addSubst n $ VarKind n' s'
    | otherwise = addSubst n' $ VarKind n s
unify (VarKind n _) kind = addSubst n kind
unify kind (VarKind n _) = addSubst n kind
unify _ _ = lift $ Left KindInferenceError

--kindInference :: DataType -> KindInferenceMonad ()
--kindInference


