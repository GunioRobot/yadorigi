
module Yadorigi.SemanticAnalysis.BindScope where

import Yadorigi.Syntax
import Yadorigi.Monad.State

import Data.Functor
import Control.Monad.State

-- Bind Scope Name

class BindScope t where
    bindScope :: t -> State Int t
    bindScope' :: t -> t
    bindScope' t = evalState (bindScope t) 0


getNextScope :: State Int Int
getNextScope = fst <$> stateTrans (1+)


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
    bindScope (ClassDecl context classname typename body) =
        return $ ClassDecl context classname typename $ bindScope' body
    bindScope (InstanceDecl context classname typename body) =
        return $ InstanceDecl context classname typename $ bindScope' body
    bindScope (BindDecl bind decls) = return $ BindDecl (bindScope' bind) (bindScope' decls)
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
    bindScope (ApplyExpr expr1 expr2) = liftM2 ApplyExpr (bindScope expr1) (bindScope expr2)
    bindScope (InfixExpr op expr1 expr2) = liftM2 (InfixExpr op) (bindScope expr1) (bindScope expr2)
    bindScope (NegativeExpr expr) = NegativeExpr <$> bindScope expr
    bindScope (ParenthesesExpr expr) = ParenthesesExpr <$> bindScope expr
    bindScope (ListExpr exprs) = ListExpr <$> bindScope exprs
    bindScope (LambdaExpr lambdas) = LambdaExpr <$> bindScope lambdas
    bindScope (LetExpr _ lets expr) = do
        scope <- getNextScope
        return $ evalState (liftM2 (LetExpr scope)
            (mapM (\(p,d) -> (,) p <$> bindScope d) lets) (bindScope expr)) 0
    bindScope (IfExpr c t f) = liftM3 IfExpr (bindScope c) (bindScope t) (bindScope f)
    bindScope (CaseExpr expr cases) = liftM2 CaseExpr (bindScope expr) (bindScope cases)
    bindScope (TypeSignatureExpr expr typename) = flip TypeSignatureExpr typename <$> bindScope expr
    bindScope expr = return expr

instance BindScope Lambda where
    bindScope (Lambda pos _ patterns expr) = do
        scope <- getNextScope
        return $ Lambda pos scope patterns (bindScope' expr)

instance BindScope CasePattern where
    bindScope (CasePattern _ pattern expr) = do
        scope <- getNextScope
        return $ CasePattern scope pattern (bindScope' expr)

