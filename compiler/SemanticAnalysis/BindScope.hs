
{-# LANGUAGE FlexibleInstances #-}

module Yadorigi.SemanticAnalysis.BindScope where

import Prelude hiding (mapM, mapM_, sequence, sequence_)
import Data.Functor
import Data.Traversable
import Control.Monad.State hiding (mapM, mapM_, sequence, sequence_)
import Control.Monad.Reader hiding (mapM, mapM_, sequence, sequence_)

import Yadorigi.Syntax
import Yadorigi.Data.Tuple.Map
import Yadorigi.Monad.State
import Yadorigi.SemanticAnalysis.DataTypes

-- Bind Scope Name

getNextScopeNum :: MonadTrans t => t (State Int) Int
getNextScopeNum = lift $ fst <$> stateTrans (1+)

getNextScope :: Int -> ReaderT Scope (State Int) Scope
getNextScope n = map2 (++[n]) <$> ask

class BindScope t where
    bindScope :: t -> ReaderT Scope (State Int) t
    bindScope' :: Scope -> t -> State Int t
    bindScope' scope t = runReaderT (bindScope t) scope
    bindScope'' :: Scope -> t -> t
    bindScope'' scope t = evalState (bindScope' scope t) 0

instance BindScope t => BindScope [t] where
    bindScope = mapM bindScope

instance BindScope Module where
    bindScope (Module modname exports imports decls) =
        return $ Module modname exports imports $ bindScope'' (modname,[]) decls

instance BindScope Decl where
    bindScope (Decl pos d) = Decl pos <$> bindScope d

instance BindScope PrimDecl where
    bindScope (DataDecl context name param body) = do
        scope <- ask
        return $ DataDecl context (rewriteScope' scope name)
            param (map (map1 (rewriteScope' scope)) body)
    bindScope (TypeDecl name param typename) = do
        scope <- ask
        return $ TypeDecl (rewriteScope' scope name) param typename
    bindScope (ClassDecl context classname typename body) = do
        scope <- ask
        ClassDecl context (rewriteScope' scope classname) typename <$> bindScope body
    bindScope (InstanceDecl context classname typename body) =
        InstanceDecl context classname typename <$> bindScope body
    --bindScope decl@(FixityDecl _ _ _) = return decl
    bindScope (TypeSignatureDecl names typename) = do
        scope <- ask
        return $ TypeSignatureDecl (map (rewriteScope' scope) names) typename
    bindScope (BindDecl _ bind decls) = do
        scopeNum <- getNextScopeNum
        scope <- getNextScope scopeNum
        return $ BindDecl scopeNum (bindScope'' scope bind) (bindScope'' scope decls)
    bindScope decl = return decl

instance BindScope Bind where
    bindScope (Bind lhs rhs) = liftM2 Bind (bindScope lhs) (bindScope rhs)

instance BindScope Lhs where
    bindScope (FunctionLhs name params) = do
        scope <- ask
        return $ FunctionLhs
            (rewriteScope' (map2 init scope) name) (map (rewriteScope' scope) params)
    bindScope (InfixLhs name pat1 pat2) = do
        scope <- ask
        return $ InfixLhs (rewriteScope' (map2 init scope) name)
            (rewriteScope' scope pat1) (rewriteScope' scope pat2)
    bindScope (PatternLhs pat) = PatternLhs <$> flip rewriteScope' pat <$> map2 init <$> ask

instance BindScope Rhs where
    bindScope (ExprRhs expr) = ExprRhs <$> bindScope expr
    bindScope (GuardRhs guards) = GuardRhs <$> bindScope guards

instance BindScope Guard where
    bindScope (Guard expr1 expr2) = liftM2 Guard (bindScope expr1) (bindScope expr2)

instance BindScope Expr where
    bindScope (Expr pos expr) = Expr pos <$> bindScope expr

instance BindScope PrimExpr where
    --bindScope expr@(LiteralExpr _) = return expr
    --bindScope expr@(NameExpr _) = return expr
    bindScope (ApplyExpr expr1 expr2) = liftM2 ApplyExpr (bindScope expr1) (bindScope expr2)
    bindScope (InfixExpr op expr1 expr2) = liftM2 (InfixExpr op) (bindScope expr1) (bindScope expr2)
    bindScope (NegativeExpr expr) = NegativeExpr <$> bindScope expr
    bindScope (ParenthesesExpr expr) = ParenthesesExpr <$> bindScope expr
    bindScope (ListExpr exprs) = ListExpr <$> bindScope exprs
    bindScope (LambdaExpr lambdas) = LambdaExpr <$> bindScope lambdas
    bindScope (LetExpr _ lets expr) = do
        scopeNum <- getNextScopeNum
        scope <- getNextScope scopeNum
        let (lets',expr') = evalState (liftM2 (,) (bindScope' scope lets) (bindScope' scope expr)) 0
        return $ LetExpr scopeNum lets' expr'
    bindScope (IfExpr c t f) = liftM3 IfExpr (bindScope c) (bindScope t) (bindScope f)
    bindScope (CaseExpr expr cases) = liftM2 CaseExpr (bindScope expr) (bindScope cases)
    bindScope (TypeSignatureExpr expr typename) = flip TypeSignatureExpr typename <$> bindScope expr
    bindScope expr = return expr

instance BindScope Lambda where
    bindScope (Lambda pos _ patterns expr) = do
        scopeNum <- getNextScopeNum
        scope <- getNextScope scopeNum
        return $ Lambda pos scopeNum patterns (bindScope'' scope expr)

instance BindScope CasePattern where
    bindScope (CasePattern _ pattern expr) = do
        scopeNum <- getNextScopeNum
        scope <- getNextScope scopeNum
        return $ CasePattern scopeNum pattern (bindScope'' scope expr)

-- Rewrite Scope

class RewriteScope t where
    rewriteScope :: t -> Reader Scope t
    rewriteScope' :: Scope -> t -> t
    rewriteScope' scope t = runReader (rewriteScope t) scope

instance (Traversable f,RewriteScope t) => RewriteScope (f t) where
    rewriteScope = mapM rewriteScope

instance RewriteScope PatternMatch where
    rewriteScope (PatternMatch pos pat) = PatternMatch pos <$> rewriteScope pat

instance RewriteScope PrimPatternMatch where
    rewriteScope (DCPattern cons pats) = DCPattern cons <$> rewriteScope pats
    --rewriteScope pat@(LiteralPattern _) = return pat
    rewriteScope (DCOpPattern op pat1 pat2) =
        liftM2 (DCOpPattern op) (rewriteScope pat1) (rewriteScope pat2)
    rewriteScope (NegativePattern pat) = NegativePattern <$> rewriteScope pat
    rewriteScope (ListPattern pats) = ListPattern <$> rewriteScope pats
    rewriteScope (BindPattern name pat) = liftM2 BindPattern (rewriteScope name) (rewriteScope pat)
    rewriteScope (ParenthesesPattern pat) = ParenthesesPattern <$> rewriteScope pat
    rewriteScope (TypeSignaturePattern pat typename) =
        flip TypeSignaturePattern typename <$> rewriteScope pat
    rewriteScope pat = return pat

instance RewriteScope ScopedName where
    rewriteScope (ScopedName _ _ name) = do
        (modname,scope) <- ask
        return $ ScopedName modname scope name

