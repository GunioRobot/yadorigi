
module Yadorigi.SemanticAnalysis.ReferModule where

import Data.Functor
import Data.List
import Data.Maybe
import Data.Tuple.All
import Control.Arrow

import Yadorigi.Common
import Yadorigi.Monad.Either
import Yadorigi.Syntax

type NameInfo = (ScopedName,[String],ModuleName)
type ModuleInfo = (ModuleName,Maybe [ExportEntity],[Import],[NameInfo],[NameInfo])
data ImportError
    = ExportConflict ModuleName String [ModuleName]
    | ModuleNotFound ModuleName ModuleName
    | ManyModuleFound ModuleName ModuleName

referModule :: [Module] -> Maybe [(ModuleName,[NameInfo])]
referModule modules =
    map (sel1&&&sel5) <$> iterateToConvergeM referModuleIter (map genModuleInfo modules)

-- get module information

genModuleInfo :: Module -> ModuleInfo
genModuleInfo (Module modname exports imports body) =
    let insideNames = nub
            [(ScopedName modname' name,children,modname) |
                (name,children) <- concatMap declToName body,modname' <- [[],modname]]
        outsideNames = filterByExportList modname exports insideNames in
            (modname,exports,Import False modname Nothing Nothing:imports,outsideNames,insideNames)

declToName :: Decl -> [(String,[String])]
declToName (Decl _ _ primdecl) = primDeclToName primdecl

primDeclToName :: PrimDecl -> [(String,[String])]
primDeclToName (DataPrimDecl _ name _ body) = [(name,nub $ map fst body)]
primDeclToName (TypePrimDecl name _ _) = [(name,[])]
primDeclToName (ClassPrimDecl _ name _ body) = [(name,nub $ concatMap (map sel1.declToName) body)]
primDeclToName (InstancePrimDecl _ name _ body) = []
primDeclToName (FixityPrimDecl _ _ _) = []
primDeclToName (TypeSignaturePrimDecl name _) = [(name,[])]
primDeclToName (BindPrimDecl (Bind lhs _) _) = lhsToName lhs

lhsToName :: Lhs -> [(String,[String])]
lhsToName (FunctionLhs name _) = [(name,[])]
lhsToName (InfixLhs name _ _) = [(name,[])]
lhsToName (PatternLhs pat) = map (id&&&const []) (patternToNames pat)

patternToNames :: PatternMatch -> [String]
patternToNames (PatternMatch _ pat) = primPatternToNames pat

primPatternToNames :: PrimPatternMatch -> [String]
primPatternToNames (DCPrimPattern _ pats) = concatMap patternToNames pats
primPatternToNames (LiteralPrimPattern _) = []
primPatternToNames (DCOpPrimPattern _ pat1 pat2) = patternToNames pat1++patternToNames pat2
primPatternToNames (NegativePrimPattern pat) = patternToNames pat
primPatternToNames (ListPrimPattern pats) = concatMap patternToNames pats
primPatternToNames (BindPrimPattern str Nothing) = [str]
primPatternToNames (BindPrimPattern str (Just pat)) = str:patternToNames pat
primPatternToNames (ParenthesesPrimPattern pat) = patternToNames pat
primPatternToNames (PrimPatternWithType pat _) = patternToNames pat

-- refer module iterator

referModuleIter :: [ModuleInfo] -> Maybe [ModuleInfo]
referModuleIter modules = mapM (referModuleIter' modules) modules

referModuleIter' :: [ModuleInfo] -> ModuleInfo -> Maybe ModuleInfo
referModuleIter' env (modname,exports,imports,outsideNames,insideNames) = do
    insideNames' <- nub <$> concat <$> mapM (importModule env) imports
    let outsideNames' = filterByExportList modname exports insideNames'
    return (modname,exports,imports,outsideNames',insideNames')

importModule :: [ModuleInfo] -> Import -> Maybe [NameInfo]
importModule env (Import qualified modname imports alias) =
    case [mod | mod <- env , modname == sel1 mod] of
        [mod] -> Just $ filterByImportList imports
            ((if qualified then id else ([]:)) [fromMaybe modname alias]) (sel4 mod)
        [] -> Nothing
        _ -> Nothing

filterByExportList :: ModuleName -> Maybe [ExportEntity] -> [NameInfo] -> [NameInfo]
filterByExportList modname exports names = nub
    [(ScopedName [] name,children,smodname) |
        export <- fromMaybe [ModuleExportEntity modname] exports,
        (ScopedName _ name,children,smodname) <- catMaybes $ map (exportFilter export) names]

exportFilter :: ExportEntity -> NameInfo -> Maybe NameInfo
exportFilter (ModuleExportEntity modname) entity@(ScopedName modname' _,_,_)
    | modname == modname' = Just entity
exportFilter (NameExportEntity (ScopedName modname name) childrenExports)
    (ScopedName modname' name',children,modname'')
    | name == name' && (null modname || modname == modname') =
        Just (ScopedName modname' name',filteredChildren,modname'')
    where filteredChildren = nub [name | export <- fromMaybe children childrenExports,
              name <- children, export == name]
exportFilter _ _ = Nothing

filterByImportList ::
    Maybe (Bool,[ImportEntity]) -> [ModuleName] -> [NameInfo] -> [NameInfo]
filterByImportList Nothing modnames names =
    [(ScopedName modname name,children,smodname) |
        (ScopedName _ name,children,smodname) <- names,
        modname <- modnames]
filterByImportList (Just (False,imports)) modnames names = writeLater

