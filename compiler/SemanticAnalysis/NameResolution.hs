
module Yadorigi.SemanticAnalysis.NameResolution where

import Data.Functor
import Data.Tuple.All
import Control.Monad.Reader
--import System.IO.Unsafe

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
    --let !_ = unsafePerformIO $ do
    --        putStrLn $ (modname>>=(++"."))++name
    --        putStrLn "tnameEnv : " >> print tnameEnv
    case [n | n <- tnameEnv, modname == sel1 n, name == sel3 n] of
        [n] -> return $ ScopedName (sel2 n) [] name
        _ -> lift $ Left NameResolutionError

varNameResolution :: ScopedName ->
    ReaderT (Scope,[GNameEnv],[GNameEnv],[LNameEnv]) (Either NameResolutionError) ScopedName
varNameResolution (ScopedName modname _ name) = do
    (_,_,gnameEnv,lnameEnv) <- ask
    --let !_ = unsafePerformIO $ do
    --        putStrLn $ (modname>>=(++"."))++name
    --        putStrLn "gnameEnv : " >> print gnameEnv
    --        putStrLn "lnameEnv : " >> print lnameEnv
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

instance NameResolution a => NameResolution [a] where
    nameResolution = mapM nameResolution

instance NameResolution Decl where
    nameResolution (Decl pos scopeNum decl) = do
        env@((modname,scope),_,_,_) <- ask
        Decl pos scopeNum <$> nameResolutionT (upd1 (modname,scope++[scopeNum]) env) decl

instance NameResolution Module where
    nameResolution (Module modname exports imports decls) =
        Module modname exports imports <$> nameResolution decls

instance NameResolution PrimDecl where
    nameResolution (DataPrimDecl context name param body) = do
        context' <- nameResolution context
        body' <- mapM (\(c,p) -> nameResolution p >>= \p' -> return (c,p')) body
        return $ DataPrimDecl context' name param body'
    nameResolution (TypePrimDecl name param typeName) =
        TypePrimDecl name param <$> nameResolution typeName
    nameResolution (ClassPrimDecl context name param decls) = do
        context' <- nameResolution context
        decls' <- nameResolution decls
        return $ ClassPrimDecl context' name param decls'
    nameResolution (InstancePrimDecl context name typeName decls) = do
        name' <- typeNameResolution name
        decls' <- nameResolution decls
        return $ InstancePrimDecl context name' typeName decls'
    --nameResolution decl@(FixityPrimDecl _ _ _) = return decl
    nameResolution (TypeSignaturePrimDecl name typeName) =
        TypeSignaturePrimDecl name <$> nameResolution typeName
    nameResolution (BindPrimDecl bind@(Bind lhs _) whereClause) = do
        (scope,tnameEnv,gnameEnv,lnameEnv) <- ask
        let lnameEnv' = overwriteNameEnv scope lnameEnv
                (lhsToIName lhs++concatMap declToName whereClause)
            newSt = (scope,tnameEnv,gnameEnv,lnameEnv')
        liftM2 BindPrimDecl (nameResolutionT newSt bind) (nameResolutionT newSt whereClause)
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
    --nameResolution expr@(LiteralPrimExpr literal) = return expr
    nameResolution (NamePrimExpr name) = NamePrimExpr <$> varNameResolution name
    nameResolution (ApplyPrimExpr func param) =
        liftM2 ApplyPrimExpr (nameResolution func) (nameResolution param)
    nameResolution (InfixPrimExpr op expr1 expr2) =
        liftM3 InfixPrimExpr (varNameResolution op) (nameResolution expr1) (nameResolution expr2)
    nameResolution (NegativePrimExpr expr) = NegativePrimExpr <$> nameResolution expr
    nameResolution (ParenthesesPrimExpr expr) = ParenthesesPrimExpr <$> nameResolution expr
    nameResolution (ListPrimExpr expr) = ListPrimExpr <$> nameResolution expr
    nameResolution (LambdaPrimExpr lambdas) = LambdaPrimExpr <$> nameResolution lambdas
    nameResolution (LetPrimExpr scopeNum lets expr) = do
        ((modname,scope),tnameEnv,gnameEnv,lnameEnv) <- ask
        let newScope = (modname,scope++[scopeNum])
            lnameEnv' = overwriteNameEnv newScope lnameEnv (concatMap (primDeclToName.snd) lets)
            newSt = (newScope,tnameEnv,gnameEnv,lnameEnv')
        liftM2 (LetPrimExpr scopeNum)
            (mapM (\(p,d) -> (,) p <$> nameResolutionT newSt d) lets) (nameResolutionT newSt expr)
    nameResolution (IfPrimExpr cond expr1 expr2) =
        liftM3 IfPrimExpr (nameResolution cond) (nameResolution expr1) (nameResolution expr2)
    nameResolution (CasePrimExpr expr pats) =
        liftM2 CasePrimExpr (nameResolution expr) (nameResolution pats)
    nameResolution (TypeSignaturePrimExpr expr typename) =
        liftM2 TypeSignaturePrimExpr (nameResolution expr) (nameResolution typename)
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
    nameResolution (DCPrimPattern cons pats) =
        liftM2 DCPrimPattern (varNameResolution cons) (nameResolution pats)
    --nameResolution pat@(LiteralPrimPattern literal) = return pat
    nameResolution (DCOpPrimPattern op pat1 pat2) =
        liftM3 DCOpPrimPattern (varNameResolution op) (nameResolution pat1) (nameResolution pat2)
    nameResolution (NegativePrimPattern pat) = NegativePrimPattern <$> nameResolution pat
    nameResolution (ListPrimPattern pats) = ListPrimPattern <$> nameResolution pats
    --nameResolution pat@(BindPrimPattern str Nothing) = return pat
    nameResolution (BindPrimPattern str (Just pat)) =
        (BindPrimPattern str.Just) <$> nameResolution pat
    nameResolution (ParenthesesPrimPattern pat) = ParenthesesPrimPattern <$> nameResolution pat
    nameResolution (PrimPatternWithType pat typename) =
        liftM2 PrimPatternWithType (nameResolution pat) (nameResolution typename)
    --nameResolution pat@PrimWildCardPattern = return pat
    nameResolution pat = return pat

instance NameResolution DataTypeWithContext where
    nameResolution (DataTypeWithContext pos context typename) =
        liftM2 (DataTypeWithContext pos) (nameResolution context) (nameResolution typename)

instance NameResolution TypeContext where
    nameResolution (TypeContext pos name typename) =
        liftM2 (TypeContext pos) (typeNameResolution name) (nameResolution typename)

instance NameResolution DataType where
    nameResolution (DataType pos typename) = DataType pos <$> nameResolution typename

instance NameResolution PrimDataType where
    --nameResolution typename@(VariablePrimType str) = return typename
    nameResolution (ConstructorPrimType cons) = ConstructorPrimType <$> typeNameResolution cons
    --nameResolution typename(ReservedConstructorPrimType str) = return typename
    nameResolution (ApplyPrimType typename1 typename2) =
        liftM2 ApplyPrimType (nameResolution typename1) (nameResolution typename2)
    nameResolution (ListPrimType typename) = ListPrimType <$> nameResolution typename
    nameResolution (FunctionPrimType typename1 typename2) =
        liftM2 FunctionPrimType (nameResolution typename1) (nameResolution typename2)
    nameResolution (ParenthesesPrimType typename) = ParenthesesPrimType <$> nameResolution typename
    nameResolution typename = return typename

