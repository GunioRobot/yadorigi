
{-# LANGUAGE FlexibleInstances #-}

module Yadorigi.SemanticAnalysis.NameResolution where

import Prelude hiding (mapM, sequence)
import Data.Functor
import Data.Traversable
import Data.Tuple.All
import Control.Monad.Reader hiding (mapM, sequence)

import Yadorigi.Monad.Either
import Yadorigi.Syntax
import Yadorigi.SemanticAnalysis.Common
import Yadorigi.SemanticAnalysis.ReferModule

data NameResolutionError = NameResolutionError deriving Show

type Scope = (ModuleName,[Int])
type GNameEnv = (ModuleName,ModuleName,String) -- Global Name Environment
type LNameEnv = (ModuleName,[Int],String) -- Local Name Environment

nameResolutionModule ::
    (Module,ModuleName,[NameInfo NameWithModule],[TypeNameInfo NameWithModule]) ->
    Either NameResolutionError Module
nameResolutionModule (mod,modname,names,types) =
    let names' = [(modname,smodname,name) | ((modname,name),smodname) <- names]++
            [(modname,smodname,name) | ((modname,_),children,smodname) <- types, name <- children]
        gnames = [name | name <- names', sel1 name == []]
        lnames = [(smodname,[],name) | (modname,smodname,name) <- names', null $ modname]
        types' = [(modname,smodname,name) | ((modname,name),_,smodname) <- types] in
            nameResolution' ((modname,[]),gnames,types',lnames) mod

overwriteNameEnv :: Scope -> [LNameEnv] -> ModuleName -> [LNameEnv]
overwriteNameEnv (modname,scope) list names = foldl overwriteIter list names
    where
        overwriteIter :: [LNameEnv] -> String -> [LNameEnv]
        overwriteIter list name = (modname,scope,name):filter ((name/=).sel3) list

typeNameResolution :: ScopedName ->
    ReaderT (Scope,[GNameEnv],[GNameEnv],[LNameEnv]) (Either NameResolutionError) ScopedName
typeNameResolution (ScopedName modname _ name) = do
    (_,tnameEnv,_,_) <- ask
    case [n | n <- tnameEnv, modname == sel1 n, name == sel3 n] of
        [n] -> return $ ScopedName (sel2 n) [] name
        _ -> lift $ Left NameResolutionError

varNameResolution :: ScopedName ->
    ReaderT (Scope,[GNameEnv],[GNameEnv],[LNameEnv]) (Either NameResolutionError) ScopedName
varNameResolution (ScopedName modname _ name) = do
    (_,_,gnameEnv,lnameEnv) <- ask
    let lsearch = case [n | n <- lnameEnv, name == sel3 n] of
            [(modname',scope,_)] -> return $ ScopedName modname' scope name
            _ -> Left NameResolutionError
        gsearch = case [n | n <- gnameEnv, modname == sel1 n, name == sel3 n] of
            [n] -> return $ ScopedName (sel2 n) [] name
            _ -> Left NameResolutionError
    lift (if null modname then lsearch else gsearch)

class NameResolution a where
    nameResolution ::
        a -> ReaderT (Scope,[GNameEnv],[GNameEnv],[LNameEnv]) (Either NameResolutionError) a
    nameResolution' :: (Scope,[GNameEnv],[GNameEnv],[LNameEnv]) -> a -> Either NameResolutionError a
    nameResolution' st a = runReaderT (nameResolution a) st
    nameResolutionT :: MonadTrans t =>
        (Scope,[GNameEnv],[GNameEnv],[LNameEnv]) -> a -> t (Either NameResolutionError) a
    nameResolutionT st a = lift $ runReaderT (nameResolution a) st

instance (Traversable f,NameResolution a) => NameResolution (f a) where
    nameResolution = mapM nameResolution

instance NameResolution Decl where
    nameResolution (Decl pos scopeNum decl) = do
        env@((modname,scope),_,_,_) <- ask
        Decl pos scopeNum <$> nameResolutionT (upd1 (modname,scope++[scopeNum]) env) decl

instance NameResolution Module where
    nameResolution (Module modname exports imports decls) =
        Module modname exports imports <$> nameResolution decls

instance NameResolution PrimDecl where
    nameResolution (DataDecl context name param body) = do
        context' <- nameResolution context
        body' <- mapM (\(c,p) -> nameResolution p >>= \p' -> return (c,p')) body
        return $ DataDecl context' name param body'
    nameResolution (TypeDecl name param typeName) = TypeDecl name param <$> nameResolution typeName
    nameResolution (ClassDecl context name param decls) = do
        context' <- nameResolution context
        decls' <- nameResolution decls
        return $ ClassDecl context' name param decls'
    nameResolution (InstanceDecl context name typeName decls) = do
        name' <- typeNameResolution name
        decls' <- nameResolution decls
        return $ InstanceDecl context name' typeName decls'
    --nameResolution decl@(FixityDecl _ _ _) = return decl
    nameResolution (TypeSignatureDecl name typeName) =
        TypeSignatureDecl name <$> nameResolution typeName
    nameResolution (BindDecl bind@(Bind lhs _) whereClause) = do
        (scope,tnameEnv,gnameEnv,lnameEnv) <- ask
        let lnameEnv' = overwriteNameEnv scope lnameEnv
                (lhsToIName lhs++concatMap declToName whereClause)
            newSt = (scope,tnameEnv,gnameEnv,lnameEnv')
        liftM2 BindDecl (nameResolutionT newSt bind) (nameResolutionT newSt whereClause)
    nameResolution decl = return decl

instance NameResolution Bind where
    nameResolution (Bind lhs rhs) = liftM2 Bind (nameResolution lhs) (nameResolution rhs)

instance NameResolution Lhs where
    nameResolution (FunctionLhs name pats) = FunctionLhs name <$> nameResolution pats
    nameResolution (InfixLhs name pat1 pat2) =
        liftM2 (InfixLhs name) (nameResolution pat1) (nameResolution pat2)
    nameResolution (PatternLhs pat) = PatternLhs <$> nameResolution pat

instance NameResolution Rhs where
    nameResolution (ExprRhs expr) = ExprRhs <$> nameResolution expr
    nameResolution (GuardRhs guard) = GuardRhs <$> nameResolution guard

instance NameResolution Guard where
    nameResolution (Guard cond expr) = liftM2 Guard (nameResolution cond) (nameResolution expr)

instance NameResolution Expr where
    nameResolution (Expr pos expr) = Expr pos <$> nameResolution expr

instance NameResolution PrimExpr where
    --nameResolution expr@(LiteralExpr _) = return expr
    nameResolution (NameExpr name) = NameExpr <$> varNameResolution name
    nameResolution (ApplyExpr func param) =
        liftM2 ApplyExpr (nameResolution func) (nameResolution param)
    nameResolution (InfixExpr op expr1 expr2) =
        liftM3 InfixExpr (varNameResolution op) (nameResolution expr1) (nameResolution expr2)
    nameResolution (NegativeExpr expr) = NegativeExpr <$> nameResolution expr
    nameResolution (ParenthesesExpr expr) = ParenthesesExpr <$> nameResolution expr
    nameResolution (ListExpr expr) = ListExpr <$> nameResolution expr
    nameResolution (LambdaExpr lambdas) = LambdaExpr <$> nameResolution lambdas
    nameResolution (LetExpr scopeNum lets expr) = do
        ((modname,scope),tnameEnv,gnameEnv,lnameEnv) <- ask
        let newScope = (modname,scope++[scopeNum])
            lnameEnv' = overwriteNameEnv newScope lnameEnv (concatMap (primDeclToName.snd) lets)
            newSt = (newScope,tnameEnv,gnameEnv,lnameEnv')
        liftM2 (LetExpr scopeNum)
            (mapM (\(p,d) -> (,) p <$> nameResolutionT newSt d) lets) (nameResolutionT newSt expr)
    nameResolution (IfExpr cond expr1 expr2) =
        liftM3 IfExpr (nameResolution cond) (nameResolution expr1) (nameResolution expr2)
    nameResolution (CaseExpr expr pats) =
        liftM2 CaseExpr (nameResolution expr) (nameResolution pats)
    nameResolution (TypeSignatureExpr expr typename) =
        liftM2 TypeSignatureExpr (nameResolution expr) (nameResolution typename)
    nameResolution expr = return expr

instance NameResolution Lambda where
    nameResolution (Lambda pos scopeNum params expr) = do
        ((modname,scope),tnameEnv,gnameEnv,lnameEnv) <- ask
        let newScope = (modname,scope++[scopeNum])
            lnameEnv' = overwriteNameEnv newScope lnameEnv (concatMap patternToNames params)
            newSt = (newScope,tnameEnv,gnameEnv,lnameEnv')
        liftM2 (Lambda pos scopeNum) (nameResolutionT newSt params) (nameResolutionT newSt expr)

instance NameResolution CasePattern where
    nameResolution (CasePattern scopeNum pat expr) = do
        ((modname,scope),tnameEnv,gnameEnv,lnameEnv) <- ask
        let newScope = (modname,scope++[scopeNum])
            lnameEnv' = overwriteNameEnv newScope lnameEnv (patternToNames pat)
            newSt = (newScope,tnameEnv,gnameEnv,lnameEnv')
        liftM2 (CasePattern scopeNum) (nameResolutionT newSt pat) (nameResolutionT newSt expr)

instance NameResolution PatternMatch where
    nameResolution (PatternMatch pos pat) = PatternMatch pos <$> nameResolution pat

instance NameResolution PrimPatternMatch where
    nameResolution (DCPattern cons pats) =
        liftM2 DCPattern (varNameResolution cons) (nameResolution pats)
    --nameResolution pat@(LiteralPattern _) = return pat
    nameResolution (DCOpPattern op pat1 pat2) =
        liftM3 DCOpPattern (varNameResolution op) (nameResolution pat1) (nameResolution pat2)
    nameResolution (NegativePattern pat) = NegativePattern <$> nameResolution pat
    nameResolution (ListPattern pats) = ListPattern <$> nameResolution pats
    nameResolution (BindPattern str pat) = BindPattern str <$> nameResolution pat
    nameResolution (ParenthesesPattern pat) = ParenthesesPattern <$> nameResolution pat
    nameResolution (PatternWithType pat typename) =
        liftM2 PatternWithType (nameResolution pat) (nameResolution typename)
    --nameResolution pat@WildCardPattern = return pat
    nameResolution pat = return pat

instance NameResolution DataTypeWithContext where
    nameResolution (DataTypeWithContext pos context typename) =
        liftM2 (DataTypeWithContext pos) (nameResolution context) (nameResolution typename)

instance NameResolution TypeContext where
    nameResolution (TypeContext cls instanceName instanceNum) =
        liftM3 TypeContext (typeNameResolution cls) (return instanceName) (return instanceNum)

instance NameResolution DataType where
    --nameResolution typename@(VarType _ _) = return typename
    nameResolution (ConstructorType kind cons) = ConstructorType kind <$> typeNameResolution cons
    --nameResolution typename(ReservedConstructorType _ _) = return typename
    nameResolution (ApplyType typename1 typename2) =
        liftM2 ApplyType (nameResolution typename1) (nameResolution typename2)
    nameResolution (ListType typename) = ListType <$> nameResolution typename
    nameResolution (FunctionType typename1 typename2) =
        liftM2 FunctionType (nameResolution typename1) (nameResolution typename2)
    nameResolution (ParenthesesType typename) = ParenthesesType <$> nameResolution typename
    nameResolution typename = return typename

