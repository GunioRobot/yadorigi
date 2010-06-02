
{-# LANGUAGE FlexibleInstances #-}

module Yadorigi.Typing.KindInference where

import Prelude hiding (foldl, foldl1, foldr, foldr1, mapM, mapM_, sequence, sequence_,
    concat, concatMap, and, or, any, all, sum, product, maximum, minimum, elem, notElem)
import Data.List hiding (foldl, foldl1, foldr, foldr1, concatMap, elem, notElem)
import Data.Maybe
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

lookupSubst :: Int -> KindInferenceMonad (Maybe Kind)
lookupSubst n = IMap.lookup n <$> sel1 <$> get

lookupAssump :: (ModuleName,String) -> KindInferenceMonad (Maybe Kind)
lookupAssump name = Map.lookup name <$> sel2 <$> get

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

infNullaryTypeCons :: KindInference' a => a -> KindInferenceMonad a
infNullaryTypeCons typename = do
    (typename',kind) <- kindInf' typename
    unify' kind AstKind
    return typename'

class KindInference a where
    kindInf :: a -> KindInferenceMonad a

class KindInference' a where
    kindInf' :: a -> KindInferenceMonad (a,Kind)

instance (Traversable f,KindInference a) => KindInference (f a) where
    kindInf = mapM kindInf

instance KindInference Module where
    kindInf (Module modname exports imports decls) =
        Module modname exports imports <$> kindInf decls

instance KindInference Decl where
    kindInf (Decl pos decl) = Decl pos <$> kindInf decl

instance KindInference PrimDecl where
    kindInf (DataDecl context name@(ScopedName m _ n) params body) = do
        context' <- kindInf context
        params' <- mapM (mapM2 kindInf) params
        body' <- mapM (mapM2 (mapM infNullaryTypeCons)) body
        addAssump (m,n) (foldr FuncKind AstKind (map sel2 params'))
        return $ DataDecl context' name params' body
    kindInf (TypeDecl name@(ScopedName m _ n) params typename) = do
        params' <- mapM (mapM2 kindInf) params
        (typename',kind) <- kindInf' typename
        addAssump (m,n) (foldr FuncKind kind (map sel2 params'))
        return $ TypeDecl name params' typename'
    kindInf (ClassDecl context name@(ScopedName m _ n) param decls) = do
        context' <- kindInf context
        param'@(_,kind) <- mapM2 kindInf param
        decls' <- kindInf decls
        addAssump (m,n) kind
        return $ ClassDecl context' name param' decls'
    kindInf (InstanceDecl context name@(ScopedName m _ n) typename decls) = do
        context' <- kindInf context
        (typename',kind) <- kindInf' typename
        decls' <- kindInf decls
        lookupAssump (m,n) >>= mapM (unify' kind)
        return $ InstanceDecl context' name typename' decls'
    --kindInf decl@(FixityDecl _ _ _) = return decl
    kindInf (TypeSignatureDecl name typename) =
        TypeSignatureDecl name <$> infNullaryTypeCons typename
    kindInf (BindDecl scopeNum bind whereClause) =
        liftM2 (BindDecl scopeNum) (kindInf bind) (kindInf whereClause)
    kindInf decl = return decl

instance KindInference Bind where
    kindInf (Bind lhs rhs) = liftM2 Bind (kindInf lhs) (kindInf rhs)

instance KindInference Lhs where
    kindInf (FunctionLhs name param) = FunctionLhs name <$> kindInf param
    kindInf (InfixLhs name pat1 pat2) = liftM2 (InfixLhs name) (kindInf pat1) (kindInf pat2)
    kindInf (PatternLhs pat) = PatternLhs <$> kindInf pat

instance KindInference Rhs where
    kindInf (ExprRhs expr) = ExprRhs <$> kindInf expr
    kindInf (GuardRhs guard) = GuardRhs <$> kindInf guard

instance KindInference Guard where
    kindInf (Guard cond expr) = liftM2 Guard (kindInf cond) (kindInf expr)

instance KindInference Expr where
    kindInf (Expr pos expr) = Expr pos <$> kindInf expr

instance KindInference PrimExpr where
    --kindInf expe@(LiteralExpr _) = return expr
    --kindInf expe@(NameExpr _) = return expr
    kindInf (ApplyExpr expr1 expr2) = liftM2 ApplyExpr (kindInf expr1) (kindInf expr2)
    kindInf (InfixExpr op left right) = liftM2 (InfixExpr op) (kindInf left) (kindInf right)
    kindInf (NegativeExpr expr) = NegativeExpr <$> kindInf expr
    kindInf (ParenthesesExpr expr) = ParenthesesExpr <$> kindInf expr
    kindInf (ListExpr exprs) = ListExpr <$> kindInf exprs
    kindInf (LambdaExpr lambda) = LambdaExpr <$> kindInf lambda
    kindInf (LetExpr scopeNum lets expr) = liftM2 (LetExpr scopeNum) (kindInf lets) (kindInf expr)
    kindInf (IfExpr c t f) = liftM3 IfExpr (kindInf c) (kindInf t) (kindInf f)
    kindInf (CaseExpr expr pat) = liftM2 CaseExpr (kindInf expr) (kindInf pat)
    kindInf (TypeSignatureExpr expr typename) =
        liftM2 TypeSignatureExpr (kindInf expr) (infNullaryTypeCons typename)
    kindInf expr = return expr

instance KindInference Lambda where
    kindInf (Lambda pos scopeNum param expr) =
        liftM2 (Lambda pos scopeNum) (kindInf param) (kindInf expr)

instance KindInference CasePattern where
    kindInf (CasePattern scopeNum pat rhs) =
        liftM2 (CasePattern scopeNum) (kindInf pat) (kindInf rhs)

instance KindInference PatternMatch where
    kindInf (PatternMatch pos pattern) = PatternMatch pos <$> kindInf pattern

instance KindInference PrimPatternMatch where
    kindInf (DCPattern name pat) = DCPattern name <$> kindInf pat
    --kindInf pat@(LiteralPattern _) = return pat
    kindInf (DCOpPattern op pat1 pat2) = liftM2 (DCOpPattern op) (kindInf pat1) (kindInf pat2)
    kindInf (NegativePattern pat) = NegativePattern <$> kindInf pat
    kindInf (ListPattern pat) = ListPattern <$> kindInf pat
    kindInf (BindPattern name pat) = BindPattern name <$> kindInf pat
    kindInf (ParenthesesPattern pat) = ParenthesesPattern <$> kindInf pat
    kindInf (TypeSignaturePattern pat typename) =
        liftM2 TypeSignaturePattern (kindInf pat) (infNullaryTypeCons typename)
    kindInf pat = return pat

instance KindInference' QualDataType where
    kindInf' (QualDataType pos context typename) = do
        context' <- kindInf context
        (typename',kind) <- kindInf' typename
        return (QualDataType pos context' typename',kind)

instance KindInference TypeContext where
    kindInf (TypeContext name@(ScopedName m _ n) typename kind) =
        TypeContext name typename <$> (lookupAssump (m,n) >>= maybe (return kind) (unify kind))

instance KindInference' DataType where
    kindInf' (VarType kind str) = do
        kind' <- kindInf kind
        return (VarType kind' str,kind')
    kindInf' (ConstructorType kind name) = do
        kind' <- kindInf kind
        return (ConstructorType kind' name,kind')
    kindInf' (ReservedType kind str) = do
        kind' <- kindInf kind
        return (ReservedType kind' str,kind')
    kindInf' (ApplyType cons param) = do
        (cons',ckind) <- kindInf' cons
        (param',pkind) <- kindInf' param
        (,) (ApplyType cons' param') <$> case ckind of
            (FuncKind f t) -> unify' f pkind >> return t
            _ -> (\(FuncKind _ t) -> t) <$>
                (flip FuncKind pkind <$> flip VarKind "" <$> newKindVar >>= unify ckind)
    kindInf' (ListType typename) = flip (,) AstKind <$> infNullaryTypeCons typename
    kindInf' (FunctionType t1 t2) =
        flip (,) AstKind <$> liftM2 FunctionType (infNullaryTypeCons t1) (infNullaryTypeCons t2)
    kindInf' (ParenthesesType typename) = kindInf' typename

instance KindInference Kind where
    kindInf AstKind = return AstKind
    kindInf (FuncKind a b) = liftM2 FuncKind (kindInf a) (kindInf b)
    kindInf kind@(VarKind n _) = fromMaybe kind <$> lookupSubst n

