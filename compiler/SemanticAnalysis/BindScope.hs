
module Yadorigi.SemanticAnalysis.BindScope where

import Data.Functor
import Data.List
import Control.Arrow
import Control.Monad.State

import Yadorigi.Syntax

-- Bind Scope Name

class BindScope t where
    bindScope :: t -> State [String] t
    bindScope_ :: [String] -> t -> t
    bindScope_ strs t = evalState (bindScope t) strs


declScopeList :: [String]
declScopeList = map (("\\dcl"++).show) [0..]

localScopeList :: [String]
localScopeList = map (("\\scp"++).show) [0..]

getNextScope :: State [String] String
getNextScope = get >>= \(x:xs) -> put xs >> return x


instance BindScope t => BindScope [t] where
    bindScope = sequence.map bindScope


instance BindScope Module where
    bindScope (Module modname imports decls) =
        return $ Module modname imports $ bindScope_ declScopeList decls


instance BindScope Decl where
    bindScope (Decl pos "" d) = do
        scope <- getNextScope
        Decl pos scope <$> bindScope d
    bindScope (Decl pos scope decl) = return $ Decl pos scope $ bindScope_ localScopeList decl

instance BindScope PrimDecl where
    bindScope (ClassPrimDecl context classname typename body) =
        return $ ClassPrimDecl context classname typename $ bindScope_ declScopeList body
    bindScope (InstancePrimDecl context classname typename body) =
        return $ InstancePrimDecl context classname typename $ bindScope_ declScopeList body
    bindScope (BindPrimDecl bind decls) =
        return $ BindPrimDecl (bindScope_ localScopeList bind) (bindScope_ declScopeList decls)
    bindScope decl = return decl


instance BindScope Bind where
    bindScope (Bind lhs rhs) = return $ Bind lhs (bindScope_ localScopeList rhs)

instance BindScope Rhs where
    bindScope (ExprRhs expr) = ExprRhs <$> bindScope expr
    bindScope (GuardRhs guards) = GuardRhs <$> bindScope guards

instance BindScope Guard where
    bindScope (Guard expr1 expr2) = liftM2 Guard (bindScope expr1) (bindScope expr2)


instance BindScope Expr where
    bindScope (Expr pos expr) = Expr pos <$> bindScope expr

instance BindScope PrimExpr where
    bindScope (ApplyFunctionPrimExpr expr1 expr2) =
        liftM2 ApplyFunctionPrimExpr (bindScope expr1) (bindScope expr2)
    bindScope (InfixPrimExpr op expr1 expr2) =
        liftM2 (InfixPrimExpr op) (bindScope expr1) (bindScope expr2)
    bindScope (NegativePrimExpr expr) = NegativePrimExpr <$> bindScope expr
    bindScope (ParenthesesPrimExpr expr) = ParenthesesPrimExpr <$> bindScope expr
    bindScope (ListPrimExpr exprs) = ListPrimExpr <$> bindScope exprs
    bindScope (LambdaPrimExpr lambdas) = LambdaPrimExpr <$> bindScope lambdas
    bindScope (LetPrimExpr "" lets expr) = do
        scope <- getNextScope
        let (lets',expr') =
                evalState (liftM2 (,) (bindScope lets) (bindScope expr)) localScopeList
        return $ LetPrimExpr scope lets' expr'
    bindScope (LetPrimExpr scope lets expr) = do
        let (lets',expr') =
                evalState (liftM2 (,) (bindScope lets) (bindScope expr)) localScopeList
        return $ LetPrimExpr scope lets' expr'
    bindScope (IfPrimExpr c t f) = liftM3 IfPrimExpr (bindScope c) (bindScope t) (bindScope f)
    bindScope (CasePrimExpr expr cases) = liftM2 CasePrimExpr (bindScope expr) (bindScope cases)
    bindScope (TypeSignaturePrimExpr expr typename) =
        flip TypeSignaturePrimExpr typename <$> bindScope expr
    bindScope expr = return expr

instance BindScope Lambda where
    bindScope (Lambda pos "" patterns expr) = do
        scope <- getNextScope
        return $ Lambda pos scope patterns (bindScope_ localScopeList expr)
    bindScope (Lambda pos scope patterns expr) =
        Lambda pos scope patterns <$> bindScope expr

instance BindScope LetDecl where
    bindScope (LetDecl pos decl) = LetDecl pos <$> bindScope decl

instance BindScope CasePattern where
    bindScope (CasePattern "" pattern expr) = do
        scope <- getNextScope
        return $ CasePattern scope pattern (bindScope_ localScopeList expr)
    bindScope (CasePattern scope pattern expr) =
        return $ CasePattern scope pattern (bindScope_ localScopeList expr)

