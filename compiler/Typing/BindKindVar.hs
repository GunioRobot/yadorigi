
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
getKindVar t s = maybe (lift (Left KindInferenceError)) (return.(,) s) (lookup s t)

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
    bindKindVar env (DataDecl context name param body) = do
        env' <- addKindVarEnv env $ map fst param++getTyvars context++concatMap (getTyvars.snd) body
        context' <- bindKindVar env' context
        param' <- mapM (getKindVar env'.fst) param
        body' <- mapM (mapM2 (bindKindVar env')) body
        return $ DataDecl context' name param' body'
    bindKindVar env (TypeDecl name param typename) = do
        env' <- addKindVarEnv env $ map fst param++getTyvars typename
        param' <- mapM (getKindVar env'.fst) param
        typename' <- bindKindVar env' typename
        return $ TypeDecl name param' typename'
    bindKindVar env (ClassDecl context name param body) = do
        env' <- addKindVarEnv env $ fst param:getTyvars context
        context' <- bindKindVar env' context
        param' <- getKindVar env' $ fst param
        body' <- bindKindVar env' body
        return $ ClassDecl context' name param' body'
    bindKindVar env (InstanceDecl context name param body) = do
        env' <- addKindVarEnv env $ getTyvars context++getTyvars param
        context' <- bindKindVar env' context
        param' <- bindKindVar env' param
        body' <- bindKindVar env' body
        return $ InstanceDecl context' name param' body'
    --bindKindVar env decl@(FixityDecl _ _ _) = return decl
    bindKindVar env (TypeSignatureDecl name typename) = do
        env' <- addKindVarEnv env $ getTyvars typename
        TypeSignatureDecl name <$> bindKindVar env' typename
    bindKindVar _ (BindDecl bind whereClause) =
        liftM2 BindDecl (bindKindVar' bind) (bindKindVar' whereClause)
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
    --bindKindVar _ expr@(LiteralExpr _) = return expr
    --bindKindVar _ expr@(NameExpr _) = return expr
    bindKindVar _ (ApplyExpr func param) =
        liftM2 ApplyExpr (bindKindVar' func) (bindKindVar' param)
    bindKindVar _ (InfixExpr name left right) =
        liftM2 (InfixExpr name) (bindKindVar' left) (bindKindVar' right)
    bindKindVar _ (NegativeExpr expr) = NegativeExpr <$> bindKindVar' expr
    bindKindVar _ (ParenthesesExpr expr) = ParenthesesExpr <$> bindKindVar' expr
    bindKindVar _ (ListExpr expr) = ListExpr <$> mapM bindKindVar' expr
    bindKindVar _ (LambdaExpr lambda) = LambdaExpr <$> mapM bindKindVar' lambda
    bindKindVar _ (LetExpr scope lets expr) =
        liftM2 (LetExpr scope) (mapM (mapM2 bindKindVar') lets) (bindKindVar' expr)
    bindKindVar _ (IfExpr c t f) = liftM3 IfExpr (bindKindVar' c) (bindKindVar' t) (bindKindVar' f)
    bindKindVar _ (CaseExpr expr pats) = liftM2 CaseExpr (bindKindVar' expr) (bindKindVar' pats)
    bindKindVar _ (TypeSignatureExpr expr typename) = do
        env <- addKindVarEnv [] $ getTyvars typename
        liftM2 TypeSignatureExpr (bindKindVar env expr) (bindKindVar env typename)
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
    bindKindVar _ (DCPattern name pat) = DCPattern name <$> bindKindVar' pat
    --bindKindVar _ pat@(LiteralPattern _) = return pat
    bindKindVar _ (DCOpPattern name left right) =
        liftM2 (DCOpPattern name) (bindKindVar' left) (bindKindVar' right)
    bindKindVar _ (NegativePattern pat) = NegativePattern <$> bindKindVar' pat
    bindKindVar _ (ListPattern pat) = ListPattern <$> mapM bindKindVar' pat
    bindKindVar _ (BindPattern name pat) = BindPattern name <$> bindKindVar' pat
    bindKindVar _ (ParenthesesPattern pat) = ParenthesesPattern <$> bindKindVar' pat
    bindKindVar _ (PatternWithType pat typename) = do
        env <- addKindVarEnv [] $ getTyvars typename
        liftM2 PatternWithType (bindKindVar' pat) (bindKindVar env typename)
    --bindKindVar _ pat@WildCardPattern = return pat
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

class HasTyvars a where
    getTyvars :: a -> [String]

instance HasTyvars a => HasTyvars [a] where
    getTyvars = concatMap getTyvars

instance HasTyvars DataTypeWithContext where
    getTyvars (DataTypeWithContext _ context typename) = getTyvars context++getTyvars typename

instance HasTyvars TypeContext where
    getTyvars (TypeContext _ v _) = [v]

instance HasTyvars DataType where
    getTyvars (DataType _ typename) = getTyvars typename

instance HasTyvars PrimDataType where
    getTyvars (VarType v _) = [v]
    --getTyvars (ConstructorType _) = []
    --getTyvars (ReservedConstructorType _) = []
    getTyvars (ApplyType a b) = getTyvars a++getTyvars b
    getTyvars (ListType typename) = getTyvars typename
    getTyvars (FunctionType a b) = getTyvars a++getTyvars b
    getTyvars (ParenthesesType typename) = getTyvars typename
    getTyvars _ = []

