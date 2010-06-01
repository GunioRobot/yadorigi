
{-# LANGUAGE FlexibleInstances,
             UndecidableInstances,
             OverlappingInstances #-}

module Yadorigi.Typing.KindInference where

import Prelude hiding (foldl, foldl1, foldr, foldr1, mapM, mapM_, sequence, sequence_,
    concat, concatMap, and, or, any, all, sum, product, maximum, minimum, elem, notElem)
import Data.List hiding (concatMap, elem, notElem)
import Data.Functor
import Data.Function
import Data.Foldable
import Data.Traversable
import qualified Data.Map as Map
import qualified Data.IntMap as IMap
import Data.Tuple.All
import Control.Monad.State.Lazy hiding (mapM, mapM_, sequence, sequence_)

import Yadorigi.Monad.Either
import Yadorigi.Monad.State
import Yadorigi.Data.Tuple.Map
import Yadorigi.Syntax

data KindInferenceError = KindInferenceError deriving Show

type Subst = IMap.IntMap Kind -- Substitution
type Assump = Map.Map (ModuleName,String) Kind -- Assumptions
type KindInferenceMonad = StateT (Subst,Assump,Int) (Either KindInferenceError)

addSubst :: Int -> Kind -> KindInferenceMonad Kind
addSubst n kind@(VarKind m _)
    | m >= n = return kind
addSubst n kind
    | n `elem` map fst (getKindvars kind) = lift $ Left KindInferenceError -- Occurs check
    | otherwise = do
        kind' <- (IMap.lookup n <$> sel1 <$> get) >>= maybe (return kind) (unify kind)
        stateTrans $ map1 $ IMap.insert n kind'
        return kind'
    where
        getKindvars :: Kind -> [(Int,String)]
        getKindvars kind = nubBy (on (==) fst) $ getKindvars' kind
        getKindvars' :: Kind -> [(Int,String)]
        getKindvars' AstKind = []
        getKindvars' (FuncKind a b) = getKindvars' a++getKindvars' b
        getKindvars' (VarKind n s) = [(n,s)]

addAssump :: (ModuleName,String) -> Kind -> KindInferenceMonad Kind
addAssump name kind = do
    kind' <- (Map.lookup name <$> sel2 <$> get) >>= maybe (return kind) (unify kind)
    stateTrans $ map2 $ Map.insert name kind'
    return kind'

newKindVar :: KindInferenceMonad Int
newKindVar = sel3 <$> fst <$> stateTrans (map3 (1+))

unify :: Kind -> Kind -> KindInferenceMonad Kind
unify AstKind AstKind = return AstKind
unify (FuncKind f a) (FuncKind g b) = liftM2 FuncKind (unify f g) (unify a b)
unify (VarKind n _) kind = addSubst n kind
unify kind (VarKind n _) = addSubst n kind
unify _ _ = lift $ Left KindInferenceError

unify' :: Kind -> Kind -> KindInferenceMonad ()
unify' AstKind AstKind = return ()
unify' (FuncKind f a) (FuncKind g b) = unify' f g >> unify' a b
unify' (VarKind n _) kind = addSubst n kind >> return ()
unify' kind (VarKind n _) = addSubst n kind >> return ()
unify' _ _ = lift $ Left KindInferenceError

match :: Kind -> Kind -> KindInferenceMonad Kind
match AstKind AstKind = return AstKind
match (FuncKind f a) (FuncKind g b) = liftM2 FuncKind (match f g) (match a b)
match (VarKind n _) kind = addSubst n kind
match _ _ = lift $ Left KindInferenceError

match' :: Kind -> Kind -> KindInferenceMonad ()
match' AstKind AstKind = return ()
match' (FuncKind f a) (FuncKind g b) = match' f g >> match' a b
match' (VarKind n _) kind = addSubst n kind >> return ()
match' _ _ = lift $ Left KindInferenceError

class KindInference a where
    unifyAll :: a -> KindInferenceMonad ()

class KindInference' a where
    unifyAll' :: a -> KindInferenceMonad Kind

instance (Traversable f,KindInference a) => KindInference (f a) where
    unifyAll = mapM_ unifyAll

instance KindInference Module where
    unifyAll (Module _ _ _ decls) = unifyAll decls

instance KindInference Decl where
    unifyAll (Decl _ decl) = unifyAll decl

instance KindInference PrimDecl where
    unifyAll (DataDecl _ _ _ body) = unifyAll $ map snd body
    unifyAll (TypeDecl _ _ typename) = unifyAll typename
    unifyAll (ClassDecl _ _ _ decls) = unifyAll decls
    unifyAll (InstanceDecl _ _ typename decls) = unifyAll typename >> unifyAll decls
    unifyAll (BindDecl _ bind whereClause) = unifyAll bind >> unifyAll whereClause
    unifyAll _ = return ()

instance KindInference Bind where
    unifyAll (Bind lhs rhs) = unifyAll lhs >> unifyAll rhs

instance KindInference Lhs where
    unifyAll (FunctionLhs _ param) = unifyAll param
    unifyAll (InfixLhs _ pat1 pat2) = unifyAll pat1 >> unifyAll pat2
    unifyAll (PatternLhs pat) = unifyAll pat

instance KindInference Rhs where
    unifyAll (ExprRhs expr) = unifyAll expr
    unifyAll (GuardRhs guard) = unifyAll guard

instance KindInference Guard where
    unifyAll (Guard cond expr) = unifyAll cond >> unifyAll expr

instance KindInference Expr where
    unifyAll (Expr _ expr) = unifyAll expr

instance KindInference PrimExpr where
    --unifyAll (LiteralExpr _) = return ()
    --unifyAll (NameExpr _) = return ()
    unifyAll (ApplyExpr expr1 expr2) = unifyAll expr1 >> unifyAll expr2
    unifyAll (InfixExpr _ left right) = unifyAll left >> unifyAll right
    unifyAll (NegativeExpr expr) = unifyAll expr
    unifyAll (ParenthesesExpr expr) = unifyAll expr
    unifyAll (ListExpr exprs) = unifyAll exprs
    unifyAll (LambdaExpr lambda) = unifyAll lambda
    unifyAll (LetExpr _ lets expr) = unifyAll lets >> unifyAll expr
    unifyAll (IfExpr c t f) = unifyAll c >> unifyAll t >> unifyAll f
    unifyAll (CaseExpr expr pat) = unifyAll expr >> unifyAll pat
    unifyAll (TypeSignatureExpr expr typename) =
        unifyAll expr >> unifyAll' typename >>= unify' AstKind
    unifyAll _ = return ()

instance KindInference Lambda where
    unifyAll (Lambda _ _ param expr) = unifyAll param >> unifyAll expr

instance KindInference CasePattern where
    unifyAll (CasePattern _ pat rhs) = unifyAll pat >> unifyAll rhs

instance KindInference PatternMatch where
    unifyAll (PatternMatch _ pattern) = unifyAll pattern

instance KindInference PrimPatternMatch where
    unifyAll (DCPattern _ pat) = unifyAll pat
    --unifyAll (LiteralPattern _) = return ()
    unifyAll (DCOpPattern _ pat1 pat2) = unifyAll pat1 >> unifyAll pat2
    unifyAll (NegativePattern pat) = unifyAll pat
    unifyAll (ListPattern pat) = unifyAll pat
    unifyAll (BindPattern _ pat) = unifyAll pat
    unifyAll (ParenthesesPattern pat) = unifyAll pat
    unifyAll (TypeSignaturePattern pat typename) =
        unifyAll pat >> unifyAll' typename >>= unify' AstKind
    unifyAll _ = return ()

--instance KindInference' a => KindInference a where
--    unifyAll a = unifyAll' a >> return ()

