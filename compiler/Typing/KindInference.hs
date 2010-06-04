
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

import Yadorigi.Common
import Yadorigi.Monad.Either
import Yadorigi.Monad.State
import Yadorigi.Data.Tuple.Map
import Yadorigi.Syntax

data KindInferenceError = KindInferenceError deriving Show

type Subst = IMap.IntMap Kind -- Substitution
type Assump = Map.Map (ModuleName,String) Kind -- Assumptions
type KindInferenceMonad = StateT (Subst,Assump,Int) (Either KindInferenceError)

addSubst :: Int -> Kind -> KindInferenceMonad Kind
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
lookupAssump name = (Map.lookup name <$> sel2 <$> get) >>= mapM renameKind

newKindVar :: KindInferenceMonad Int
newKindVar = sel3 <$> fst <$> stateTrans (map3 (1+))

unify :: Kind -> Kind -> KindInferenceMonad Kind
unify AstKind AstKind = return AstKind
unify (FuncKind f a) (FuncKind g b) = liftM2 FuncKind (unify f g) (unify a b)
unify a@(VarKind n _) b@(VarKind m _)
    | n < m = addSubst m a
    | m > n = addSubst n b
    | otherwise = return a
unify (VarKind n _) kind = addSubst n kind
unify kind (VarKind n _) = addSubst n kind
unify _ _ = lift $ Left KindInferenceError

unify' :: Kind -> Kind -> KindInferenceMonad ()
unify' AstKind AstKind = return ()
unify' (FuncKind f a) (FuncKind g b) = unify' f g >> unify' a b
unify' a@(VarKind n _) b@(VarKind m _)
    | n < m = addSubst m a >> return ()
    | m > n = addSubst n b >> return ()
    | otherwise = return ()
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

renameKind :: Kind -> KindInferenceMonad Kind
renameKind kind = fst <$> runStateT (renameKind' kind) IMap.empty
    where
        renameKind' :: Kind -> StateT (IMap.IntMap Int) KindInferenceMonad Kind
        renameKind' AstKind = return AstKind
        renameKind' (FuncKind a b) = liftM2 FuncKind (renameKind' a) (renameKind' b)
        renameKind' (VarKind n s) = do
            r <- IMap.lookup n <$> get
            flip VarKind s <$> case r of
                Nothing -> do
                    n' <- lift $ newKindVar
                    stateTrans $ IMap.insert n n'
                    return n'
                (Just n') -> return n'

infNullaryTypeCons :: KindInference' a => a -> KindInferenceMonad a
infNullaryTypeCons typename = do
    (typename',kind) <- kindInf' typename
    unify' kind AstKind
    return typename'

kindInfModules :: [Module] -> Either KindInferenceError [Module]
kindInfModules modules = fst <$>
    runStateT (bindKindVar' modules >>= iterateToConvergeST kindInf) (IMap.empty,Map.empty,0)

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
    kindInf' (ConstructorType kind name@(ScopedName m _ n)) = do
        kind' <- kindInf kind
        kind'' <- lookupAssump (m,n) >>= maybe (return kind') (unify kind')
        return (ConstructorType kind'' name,kind'')
    kindInf' (ReservedType kind str) = do
        kind' <- kindInf kind >>= unify (reservedTypeToKind str)
        return (ReservedType kind' str,kind')
        where
            reservedTypeToKind :: String -> Kind
            reservedTypeToKind "[]" = FuncKind AstKind AstKind
            reservedTypeToKind "->" = FuncKind AstKind $ FuncKind AstKind AstKind
            reservedTypeToKind _ = error "Yadorigi.Typing.KindInference : reservedTypeToKind failed"
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

-- BindKindVar

addKindVarEnv :: [(String,Int)] -> [String] -> KindInferenceMonad [(String,Int)]
addKindVarEnv env newVars = foldlM addKindVarEnv' env $ nub $ newVars \\ map fst env where
    addKindVarEnv' :: [(String,Int)] -> String -> KindInferenceMonad [(String,Int)]
    addKindVarEnv' env n = (:env) <$> (,) n <$> newKindVar

getKindVar :: [(String,Int)] -> String -> KindInferenceMonad (String,Int)
getKindVar t s = maybe (lift (Left KindInferenceError)) (return.(,) s) (lookup s t)

numToVarKind :: Int -> Kind
numToVarKind n = VarKind n ""

class BindKindVar a where
    bindKindVar :: [(String,Int)] -> a -> KindInferenceMonad a
    bindKindVar' :: a -> KindInferenceMonad a
    bindKindVar' = bindKindVar []

instance (Traversable f,BindKindVar a) => BindKindVar (f a) where
    bindKindVar env = mapM $ bindKindVar env

instance BindKindVar Module where
    bindKindVar env (Module modname exports imports decls) =
        Module modname exports imports <$> bindKindVar env decls

instance BindKindVar Decl where
    bindKindVar env (Decl pos decl) = Decl pos <$> bindKindVar env decl

instance BindKindVar PrimDecl where
    bindKindVar env (DataDecl context name param body) = do
        env' <- addKindVarEnv env $ map fst param++getTyvars context++concatMap (getTyvars.snd) body
        context' <- bindKindVar env' context
        param' <- mapM (fmap (map2 numToVarKind).getKindVar env'.fst) param
        body' <- mapM (mapM2 (bindKindVar env')) body
        return $ DataDecl context' name param' body'
    bindKindVar env (TypeDecl name param typename) = do
        env' <- addKindVarEnv env $ map fst param++getTyvars typename
        param' <- mapM (fmap (map2 numToVarKind).getKindVar env'.fst) param
        typename' <- bindKindVar env' typename
        return $ TypeDecl name param' typename'
    bindKindVar env (ClassDecl context name param body) = do
        env' <- addKindVarEnv env $ fst param:getTyvars context
        context' <- bindKindVar env' context
        param' <- map2 numToVarKind <$> getKindVar env' (fst param)
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
    bindKindVar _ (BindDecl scope bind whereClause) =
        liftM2 (BindDecl scope) (bindKindVar' bind) (bindKindVar' whereClause)
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
        liftM2 (LetExpr scope) (mapM bindKindVar' lets) (bindKindVar' expr)
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
    bindKindVar _ (TypeSignaturePattern pat typename) = do
        env <- addKindVarEnv [] $ getTyvars typename
        liftM2 TypeSignaturePattern (bindKindVar' pat) (bindKindVar env typename)
    --bindKindVar _ pat@WildCardPattern = return pat
    bindKindVar _ pat = return pat

instance BindKindVar QualDataType where
    bindKindVar env (QualDataType pos context typename) =
        liftM2 (QualDataType pos) (bindKindVar env context) (bindKindVar env typename)

instance BindKindVar TypeContext where
    bindKindVar env (TypeContext typeclass typename _) =
        TypeContext typeclass typename.flip VarKind "".snd <$> getKindVar env typename

instance BindKindVar DataType where
    bindKindVar env (VarType _ typename) =
        flip VarType typename.flip VarKind "".snd <$> getKindVar env typename
    bindKindVar env (ConstructorType _ cons) =
        flip ConstructorType cons.flip VarKind "" <$> newKindVar
    bindKindVar env (ReservedType _ cons) = flip ReservedType cons.flip VarKind "" <$> newKindVar
    bindKindVar env (ApplyType cons param) =
        liftM2 ApplyType (bindKindVar env cons) (bindKindVar env param)
    bindKindVar env (ListType typename) = ListType <$> bindKindVar env typename
    bindKindVar env (FunctionType t1 t2) =
        liftM2 FunctionType (bindKindVar env t1) (bindKindVar env t2)
    bindKindVar env (ParenthesesType typename) = ParenthesesType <$> bindKindVar env typename

class HasTyvars a where
    getTyvars :: a -> [String]

instance HasTyvars a => HasTyvars [a] where
    getTyvars = concatMap getTyvars

instance HasTyvars QualDataType where
    getTyvars (QualDataType _ context typename) = getTyvars context++getTyvars typename

instance HasTyvars TypeContext where
    getTyvars (TypeContext _ v _) = [v]

instance HasTyvars DataType where
    getTyvars (VarType _ v) = [v]
    --getTyvars (ConstructorType _) = []
    --getTyvars (ReservedType _) = []
    getTyvars (ApplyType a b) = getTyvars a++getTyvars b
    getTyvars (ListType typename) = getTyvars typename
    getTyvars (FunctionType a b) = getTyvars a++getTyvars b
    getTyvars (ParenthesesType typename) = getTyvars typename
    getTyvars _ = []

