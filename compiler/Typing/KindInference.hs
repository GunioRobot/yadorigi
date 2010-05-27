
{-# LANGUAGE FlexibleInstances #-}

module Yadorigi.Typing.KindInference where

import Yadorigi.Monad.Either
import Yadorigi.Monad.State
import Yadorigi.Data.Tuple.Map
import Yadorigi.Syntax

import Prelude hiding (foldl, foldl1, foldr, foldr1, mapM, mapM_, sequence, sequence_,
    concat, concatMap, and, or, any, all, sum, product, maximum, minimum, elem, notElem)
import Data.List hiding (concatMap, elem, notElem)
import Data.Functor
import Data.Function
import Data.Foldable
import Data.Traversable
import qualified Data.IntMap as IM
import Data.Tuple.All
import Control.Monad.State.Lazy hiding (mapM, mapM_, sequence, sequence_)

data KindInferenceError = KindInferenceError deriving Show

type TypeKindEnv = ((ModuleName,String),Kind)
type KindInferenceMonad = StateT (IM.IntMap Kind,[TypeKindEnv],Int) (Either KindInferenceError)

addSubst :: Int -> Kind -> KindInferenceMonad Kind
addSubst n kind@(VarKind m _)
    | m >= n = return kind
addSubst n kind
    | elem n (map fst (getKindvars kind)) = lift $ Left KindInferenceError -- Occurs check
    | otherwise = stateTrans (map1 (IM.insert n kind)) >> return kind

getKindvars :: Kind -> [(Int,String)]
getKindvars kind = nubBy (on (==) fst) $ getKindvars' kind
    where
        getKindvars' :: Kind -> [(Int,String)]
        getKindvars' AstKind = []
        getKindvars' (FuncKind a b) = getKindvars' a++getKindvars' b
        getKindvars' (VarKind n s) = [(n,s)]

newKindVar :: KindInferenceMonad Int
newKindVar = sel3 <$> fst <$> stateTrans (map3 (1+))

unify :: Kind -> Kind -> KindInferenceMonad Kind
unify AstKind AstKind = return AstKind
unify (FuncKind f a) (FuncKind g b) = liftM2 FuncKind (unify f g) (unify a b)
unify (VarKind n _) kind = addSubst n kind
unify kind (VarKind n _) = addSubst n kind
unify _ _ = lift $ Left KindInferenceError

match :: Kind -> Kind -> KindInferenceMonad Kind
match AstKind AstKind = return AstKind
match (FuncKind f a) (FuncKind g b) = liftM2 FuncKind (match f g) (match a b)
match (VarKind n _) kind = addSubst n kind
match _ _ = lift $ Left KindInferenceError

--class KindInference a where
--    unifyAll :: a -> KindInferenceMonad ()
--
--instance (Traversable f,KindInference a) => KindInference (f a) where
--    unifyAll = mapM_ unifyAll
--
--instance KindInference Module where
--    unifyAll (Module _ _ _ decls) = unifyAll decls
--
--instance KindInference Decl where
--    unifyAll (Decl _ _ decl) = unifyAll decl
--
--instance KindInference PrimDecl where
--    unifyAll (DataDecl _ _ _ body) = unifyAll $ map snd body
--    unifyAll (TypeDecl _ _ typename) = unifyAll typename
--    unifyAll (ClassDecl _ _ _ decls) = unifyAll decls
--    unifyAll (InstanceDecl _ _ typename decls) = unifyAll typename >> unifyAll decls
--    unifyAll (BindDecl bind whereClause) = unifyAll bind >> unifyAll whereClause
--    unifyAll _ = return ()
--
--instance KindInference Bind where
--    unifyAll (Bind lhs rhs) =
--
--class KindInference' a where
--    unifyAll' :: a -> KindInferenceMonad Kind

