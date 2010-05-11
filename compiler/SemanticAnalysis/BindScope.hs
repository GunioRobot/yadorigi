
module Yadorigi.SemanticAnalysis.BindScope where

import Data.Functor
import Control.Monad.State

import Yadorigi.Syntax

-- Bind Scope Name

class BindScope t where
    bindScope :: t -> State Int t
    bindScope' :: t -> t
    bindScope' t = evalState (bindScope t) 0


getNextScope :: State Int Int
getNextScope = get >>= \n -> put (n+1) >> return n


instance BindScope t => BindScope [t] where
    bindScope = sequence.map bindScope


instance BindScope Module where
    bindScope (Module modname exports imports decls) =
        return $ Module modname exports imports $ bindScope' decls


instance BindScope Decl where
    bindScope (Decl pos _ d) = do
        scope <- getNextScope
        Decl pos scope <$> bindScope d

instance BindScope PrimDecl where
    bindScope (ClassPrimDecl context classname typename body) =
        return $ ClassPrimDecl context classname typename $ bindScope' body
    bindScope (InstancePrimDecl context classname typename body) =
        return $ InstancePrimDecl context classname typename $ bindScope' body
    bindScope (BindPrimDecl bind decls) =
        return $ BindPrimDecl (bindScope' bind) (bindScope' decls)
    bindScope decl = return decl


instance BindScope Bind where
    bindScope (Bind lhs rhs) = return $ Bind lhs (bindScope' rhs)

instance BindScope Rhs where
    bindScope (ExprRhs expr) = ExprRhs <$> bindScope expr
    bindScope (GuardRhs guards) = GuardRhs <$> bindScope guards

instance BindScope Guard where
    bindScope (Guard expr1 expr2) = liftM2 Guard (bindScope expr1) (bindScope expr2)


instance BindScope Expr where
    bindScope (Expr pos expr) = Expr pos <$> bindScope expr

instance BindScope PrimExpr where
    bindScope (ApplyPrimExpr expr1 expr2) = liftM2 ApplyPrimExpr (bindScope expr1) (bindScope expr2)
    bindScope (InfixPrimExpr op expr1 expr2) =
        liftM2 (InfixPrimExpr op) (bindScope expr1) (bindScope expr2)
    bindScope (NegativePrimExpr expr) = NegativePrimExpr <$> bindScope expr
    bindScope (ParenthesesPrimExpr expr) = ParenthesesPrimExpr <$> bindScope expr
    bindScope (ListPrimExpr exprs) = ListPrimExpr <$> bindScope exprs
    bindScope (LambdaPrimExpr lambdas) = LambdaPrimExpr <$> bindScope lambdas
    bindScope (LetPrimExpr _ lets expr) = do
        scope <- getNextScope
        return $ evalState (liftM2 (LetPrimExpr scope)
            (mapM (\(p,d) -> (,) p <$> bindScope d) lets) (bindScope expr)) 0
    bindScope (IfPrimExpr c t f) = liftM3 IfPrimExpr (bindScope c) (bindScope t) (bindScope f)
    bindScope (CasePrimExpr expr cases) = liftM2 CasePrimExpr (bindScope expr) (bindScope cases)
    bindScope (TypeSignaturePrimExpr expr typename) =
        flip TypeSignaturePrimExpr typename <$> bindScope expr
    bindScope expr = return expr

instance BindScope Lambda where
    bindScope (Lambda pos _ patterns expr) = do
        scope <- getNextScope
        return $ Lambda pos scope patterns (bindScope' expr)

instance BindScope CasePattern where
    bindScope (CasePattern _ pattern expr) = do
        scope <- getNextScope
        return $ CasePattern scope pattern (bindScope' expr)

