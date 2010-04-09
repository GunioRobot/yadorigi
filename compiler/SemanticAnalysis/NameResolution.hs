
module Yadorigi.SemanticAnalysis.NameResolution where

import Data.Functor
import Control.Monad

import Yadorigi.Common
import Yadorigi.Monad.Either
import Yadorigi.Syntax
import Yadorigi.SemanticAnalysis.Common

data NameResolutionError = NameResolutionError

class NameResolution a where
    nameResolution ::
        ([String],[Int]) -> [(String,[([String],[Int])])] -> a -> Either NameResolutionError a

overwriteNameEnv ::
    ([String],[Int]) -> [(String,[([String],[Int])])] -> [String] -> [(String,[([String],[Int])])]
overwriteNameEnv scope list names = foldl overwriteIter list names
    where overwriteIter list name = (name,[scope]):filter ((name /=).fst) list

instance NameResolution Decl where
    nameResolution (modname,scope) nameEnv (Decl pos scope' decl) =
       Decl pos scope' <$> nameResolution (modname,scope++[scope']) nameEnv decl

instance NameResolution PrimDecl where
    nameResolution scope nameEnv decl@(DataPrimDecl _ _ _ _) = Right decl
    nameResolution scope nameEnv decl@(TypePrimDecl _ _ _) = Right decl
    nameResolution scope nameEnv (ClassPrimDecl context name param decls) =
        ClassPrimDecl context name param <$> mapM (nameResolution scope nameEnv) decls
    nameResolution scope nameEnv (InstancePrimDecl context name typename decls) = do
        name' <- nameResolution scope nameEnv name
        decls' <- mapM (nameResolution scope nameEnv) decls
        return $ InstancePrimDecl context name' typename decls'
    nameResolution scope nameEnv decl@(FixityPrimDecl fixity num ops) = Right decl
    nameResolution scope nameEnv decl@(TypeSignaturePrimDecl str typeName) = Right decl
    nameResolution scope nameEnv (BindPrimDecl bind@(Bind lhs _) whereClause) =
        let nameEnv' = overwriteNameEnv scope nameEnv
                (lhsToParams lhs++concatMap declToName' whereClause) in
            liftM2 BindPrimDecl
                (nameResolution scope nameEnv' bind)
                (mapM (nameResolution scope nameEnv') whereClause)

instance NameResolution Bind where
    nameResolution scope nameEnv (Bind lhs rhs) = writeLater

instance NameResolution ScopedName where
    nameResolution scope nameEnv (ScopedName [] _ name) = writeLater
    nameResolution scope nameEnv (ScopedName modname _ name) = writeLater
