
{-# LANGUAGE FlexibleInstances #-}

module Yadorigi.Typing.BindKindVar where

import Yadorigi.Monad.Either
import Yadorigi.Data.Tuple.Map
import Yadorigi.Syntax
import Yadorigi.Typing.KindInference

import Prelude hiding (foldl, foldl1, foldr, foldr1, mapM, mapM_, sequence, sequence_,
    concat, concatMap, and, or, any, all, sum, product, maximum, minimum, elem, notElem)
import Data.List hiding (concatMap)
import Data.Functor
import Data.Foldable
import Data.Traversable
import qualified Data.IntMap as IM
import Control.Monad hiding (mapM, mapM_, sequence, sequence_)
import Control.Monad.Trans

addKindVarEnv :: [(String,Int)] -> [String] -> KindInferenceMonad [(String,Int)]
addKindVarEnv env newVars = foldlM addKindVarEnv' env (nub $ newVars \\ map fst env) where
    addKindVarEnv' :: [(String,Int)] -> String -> KindInferenceMonad [(String,Int)]
    addKindVarEnv' env n = (:env) <$> (,) n <$> newKindVar

getKindVar :: [(String,Int)] -> String -> KindInferenceMonad (String,Int)
getKindVar t s = case lookup s t of
    (Just n) -> return (s,n)
    Nothing -> lift $ Left KindInferenceError

class BindKindVar a where
    bindKindVar :: [(String,Int)] -> a -> KindInferenceMonad a
    bindKindVar' :: a -> KindInferenceMonad a
    bindKindVar' = bindKindVar []

instance (Traversable f,BindKindVar a) => BindKindVar (f a) where
    bindKindVar env l = mapM (bindKindVar env) l

instance BindKindVar Module where
    bindKindVar env (Module modname exports imports decls) =
        Module modname exports imports <$> bindKindVar env decls

instance BindKindVar Decl where
    bindKindVar env (Decl pos scope decl) = Decl pos scope <$> bindKindVar env decl

instance BindKindVar PrimDecl where
    bindKindVar env (DataPrimDecl context name param body) = do
        env' <- addKindVarEnv env $ map fst param++getTyvars context++concatMap (getTyvars.snd) body
        context' <- bindKindVar env' context
        param' <- mapM (getKindVar env'.fst) param
        body' <- mapM (mapM2 (bindKindVar env')) body
        return $ DataPrimDecl context' name param' body'
    bindKindVar env (TypePrimDecl name param typename) = do
        env' <- addKindVarEnv env $ map fst param++getTyvars typename
        param' <- mapM (getKindVar env'.fst) param
        typename' <- bindKindVar env' typename
        return $ TypePrimDecl name param' typename'
    bindKindVar env (ClassPrimDecl context name param body) = do
        env' <- addKindVarEnv env $ fst param:getTyvars context
        context' <- bindKindVar env' context
        param' <- getKindVar env' $ fst param
        body' <- bindKindVar env' body
        return $ ClassPrimDecl context' name param' body'
    bindKindVar env (InstancePrimDecl context name param body) = do
        env' <- addKindVarEnv env $ getTyvars context++getTyvars param
        context' <- bindKindVar env' context
        param' <- bindKindVar env' param
        body' <- bindKindVar env' body
        return $ InstancePrimDecl context' name param' body'
    --bindKindVar env decl@(FixityPrimDecl _ _ _) = return decl
    bindKindVar env (TypeSignaturePrimDecl name typename) = do
        env' <- addKindVarEnv env $ getTyvars typename
        TypeSignaturePrimDecl name <$> bindKindVar env' typename
    bindKindVar _ (BindPrimDecl bind whereClause) =
        liftM2 BindPrimDecl (bindKindVar' bind) (bindKindVar' whereClause)
    bindKindVar _ decl = return decl

instance BindKindVar Bind where
    bindKindVar _ (Bind lhs rhs) = liftM2 Bind (bindKindVar' lhs) (bindKindVar' rhs)

instance BindKindVar Lhs where
    bindKindVar _ (FunctionLhs name param) = FunctionLhs name <$> bindKindVar' param
    bindKindVar _ (InfixLhs name pat1 pat2) =
        liftM2 (InfixLhs name) (bindKindVar' pat1) (bindKindVar' pat1)
    bindKindVar _ (PatternLhs pat) = PatternLhs <$> bindKindVar' pat

instance BindKindVar Rhs where
    bindKindVar _ (ExprRhs expr) = ExprRhs <$> bindKindVar' expr
    bindKindVar _ (GuardRhs guard) = GuardRhs <$> bindKindVar' guard

instance BindKindVar Guard where
    bindKindVar _ (Guard cond expr) = liftM2 Guard (bindKindVar' cond) (bindKindVar' expr)

instance BindKindVar Expr where
    bindKindVar _ (Expr pos expr) = Expr pos <$> bindKindVar' expr

instance BindKindVar PrimExpr where
    --bindKindVar _ expr@(LiteralPrimExpr _) = return expr
    --bindKindVar _ expr@(NamePrimExpr _) = return expr
    bindKindVar _ (ApplyPrimExpr func param) =
        liftM2 ApplyPrimExpr (bindKindVar' func) (bindKindVar' param)
    bindKindVar _ (InfixPrimExpr name left right) =
        liftM2 (InfixPrimExpr name) (bindKindVar' left) (bindKindVar' right)
    bindKindVar _ (NegativePrimExpr expr) = NegativePrimExpr <$> bindKindVar' expr
    bindKindVar _ (ParenthesesPrimExpr expr) = ParenthesesPrimExpr <$> bindKindVar' expr
    bindKindVar _ (ListPrimExpr expr) = ListPrimExpr <$> mapM bindKindVar' expr
    bindKindVar _ (LambdaPrimExpr lambda) = LambdaPrimExpr <$> mapM bindKindVar' lambda
    bindKindVar _ (LetPrimExpr scope lets expr) =
        liftM2 (LetPrimExpr scope) (mapM (mapM2 bindKindVar') lets) (bindKindVar' expr)
    bindKindVar _ (IfPrimExpr c t f) =
        liftM3 IfPrimExpr (bindKindVar' c) (bindKindVar' t) (bindKindVar' f)
    bindKindVar _ (CasePrimExpr expr pats) =
        liftM2 CasePrimExpr (bindKindVar' expr) (bindKindVar' pats)
    bindKindVar _ (TypeSignaturePrimExpr expr typename) = do
        env <- addKindVarEnv [] $ getTyvars typename
        liftM2 TypeSignaturePrimExpr (bindKindVar env expr) (bindKindVar env typename)
    bindKindVar _ expr = return expr

instance BindKindVar Lambda where
    bindKindVar _ (Lambda pos scope param expr) =
        liftM2 (Lambda pos scope) (bindKindVar' param) (bindKindVar' expr)

instance BindKindVar CasePattern where
    bindKindVar _ (CasePattern scope pat rhs) =
        liftM2 (CasePattern scope) (bindKindVar' pat) (bindKindVar' rhs)

instance BindKindVar PatternMatch where
    bindKindVar _ (PatternMatch pos pat) = PatternMatch pos <$> bindKindVar' pat

instance BindKindVar PrimPatternMatch where
    bindKindVar _ (DCPrimPattern name pat) = DCPrimPattern name <$> bindKindVar' pat
    --bindKindVar _ pat@(LiteralPrimPattern _) = return pat
    bindKindVar _ (DCOpPrimPattern name left right) =
        liftM2 (DCOpPrimPattern name) (bindKindVar' left) (bindKindVar' right)
    bindKindVar _ (NegativePrimPattern pat) = NegativePrimPattern <$> bindKindVar' pat
    bindKindVar _ (ListPrimPattern pat) = ListPrimPattern <$> mapM bindKindVar' pat
    bindKindVar _ (BindPrimPattern name pat) = BindPrimPattern name <$> bindKindVar' pat
    bindKindVar _ (ParenthesesPrimPattern pat) = ParenthesesPrimPattern <$> bindKindVar' pat
    bindKindVar _ (PrimPatternWithType pat typename) = do
        env <- addKindVarEnv [] $ getTyvars typename
        liftM2 PrimPatternWithType (bindKindVar' pat) (bindKindVar env typename)
    --bindKindVar _ pat@PrimWildCardPattern = return pat
    bindKindVar _ pat = return pat

instance BindKindVar DataTypeWithContext where
    bindKindVar env (DataTypeWithContext pos context typename) =
        liftM2 (DataTypeWithContext pos) (bindKindVar env context) (bindKindVar env typename)

instance BindKindVar TypeContext where
    bindKindVar env (TypeContext typeclass typename _) =
        uncurry (TypeContext typeclass) <$> getKindVar env typename

instance BindKindVar DataType where
    bindKindVar env (DataType kind typename) = DataType kind <$> bindKindVar env typename

instance BindKindVar PrimDataType where
    bindKindVar env (VarType typename _) = uncurry VarType <$> getKindVar env typename
    --bindKindVar env typename@(ConstructorType _) = return typename
    --bindKindVar env typename@(ReservedConstructorType _) = return typename
    bindKindVar env (ApplyType cons param) =
        liftM2 ApplyType (bindKindVar env cons) (bindKindVar env param)
    bindKindVar env (ListType typename) = ListType <$> bindKindVar env typename
    bindKindVar env (FunctionType t1 t2) =
        liftM2 FunctionType (bindKindVar env t1) (bindKindVar env t2)
    bindKindVar env (ParenthesesType typename) = ParenthesesType <$> bindKindVar env typename
    bindKindVar env typename = return typename

class HaveTyvars a where
    getTyvars :: a -> [String]

instance HaveTyvars a => HaveTyvars [a] where
    getTyvars = concatMap getTyvars

instance HaveTyvars DataTypeWithContext where
    getTyvars (DataTypeWithContext _ context typename) = getTyvars context++getTyvars typename

instance HaveTyvars TypeContext where
    getTyvars (TypeContext _ v _) = [v]

instance HaveTyvars DataType where
    getTyvars (DataType _ typename) = getTyvars typename

instance HaveTyvars PrimDataType where
    getTyvars (VarType v _) = [v]
    --getTyvars (ConstructorType _) = []
    --getTyvars (ReservedConstructorType _) = []
    getTyvars (ApplyType a b) = getTyvars a++getTyvars b
    getTyvars (ListType typename) = getTyvars typename
    getTyvars (FunctionType a b) = getTyvars a++getTyvars b
    getTyvars (ParenthesesType typename) = getTyvars typename
    getTyvars _ = []

