
module Yadorigi.SemanticAnalysis.ReferModule where

import Data.Functor
import Data.List
import Data.Tuple.All
import Control.Arrow

import Yadorigi.Common
import Yadorigi.Syntax

referModule :: [Module] -> [(ModuleName,[(ScopedName,Maybe [ScopedName],Bool,ModuleName)])]
referModule modules =
    map (sel1&&&sel5) $ iterateToConverge referModuleIter $ map genModuleInfo modules


genModuleInfo :: Module ->
    (ModuleName,(Maybe [ExportEntity]),[Import]
        ,[(ScopedName,Maybe [ScopedName],Bool,ModuleName)]
        ,[(ScopedName,Maybe [ScopedName],Bool,ModuleName)])
genModuleInfo (Module modname exports imports body) =
    (modname,exports,imports,[],concatMap (declToName modname) body)

declToName :: ModuleName -> Decl -> [(ScopedName,Maybe [ScopedName],Bool,ModuleName)]
declToName modname (Decl _ _ primdecl) = primDeclToName modname primdecl

primDeclToName :: ModuleName -> PrimDecl -> [(ScopedName,Maybe [ScopedName],Bool,ModuleName)]
primDeclToName modname (DataPrimDecl _ name _ body) =
    [(ScopedName modname name,Just $ nub $ map (ScopedName modname.fst) body,False,modname)]
primDeclToName modname (TypePrimDecl name _ _) = [(ScopedName modname name,Nothing,False,modname)]
primDeclToName modname (ClassPrimDecl _ name _ body) = [(ScopedName modname name
    ,Just $ nub $ concatMap (map sel1.declToName modname) body,False,modname)]
primDeclToName modname (InstancePrimDecl _ name _ body) = []
primDeclToName modname (FixityPrimDecl _ _ _) = []
primDeclToName modname (TypeSignaturePrimDecl name _) =
    [(ScopedName modname name,Nothing,False,modname)]
primDeclToName modname (BindPrimDecl (Bind lhs _) _) = lhsToName modname lhs

lhsToName :: ModuleName -> Lhs -> [(ScopedName,Maybe [ScopedName],Bool,ModuleName)]
lhsToName modname (FunctionLhs name _) = [(ScopedName modname name,Nothing,False,modname)]
lhsToName modname (InfixLhs name _ _) = [(ScopedName modname name,Nothing,False,modname)]
lhsToName modname (PatternLhs pat) =
    map (\name -> (ScopedName modname name,Nothing,False,modname)) $ patternToNames pat

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


referModuleIter ::
    [(ModuleName,Maybe [ExportEntity],[Import]
        ,[(ScopedName,Maybe [ScopedName],Bool,ModuleName)]
        ,[(ScopedName,Maybe [ScopedName],Bool,ModuleName)])] ->
    [(ModuleName,Maybe [ExportEntity],[Import]
        ,[(ScopedName,Maybe [ScopedName],Bool,ModuleName)]
        ,[(ScopedName,Maybe [ScopedName],Bool,ModuleName)])]
referModuleIter modules = map (referModuleIter' modules) modules

referModuleIter' ::
    [(ModuleName,Maybe [ExportEntity],[Import]
        ,[(ScopedName,Maybe [ScopedName],Bool,ModuleName)]
        ,[(ScopedName,Maybe [ScopedName],Bool,ModuleName)])] ->
    (ModuleName,Maybe [ExportEntity],[Import]
        ,[(ScopedName,Maybe [ScopedName],Bool,ModuleName)]
        ,[(ScopedName,Maybe [ScopedName],Bool,ModuleName)]) ->
    (ModuleName,Maybe [ExportEntity],[Import]
        ,[(ScopedName,Maybe [ScopedName],Bool,ModuleName)]
        ,[(ScopedName,Maybe [ScopedName],Bool,ModuleName)])
referModuleIter' env (modname,exports,imports,outsideNames,insideNames) =
    let outsideNames' = nub $ insideNames++concatMap (importModule env) imports
        insideNames' = filterByExportList modname exports outsideNames' in
        (modname,exports,imports,outsideNames',insideNames')

importModule ::
    [(ModuleName,Maybe [ExportEntity],[Import]
        ,[(ScopedName,Maybe [ScopedName],Bool,ModuleName)]
        ,[(ScopedName,Maybe [ScopedName],Bool,ModuleName)])]
    -> Import -> [(ScopedName,Maybe [ScopedName],Bool,ModuleName)]
importModule env (Import qualified modname imports alias) =
    let Just (_,_,_,outsideNames,_) = find ((modname==).sel1) env in
        filterByImportList qualified imports alias outsideNames

-- ToDo : write filter
filterByExportList :: ModuleName -> Maybe [ExportEntity] ->
    [(ScopedName,Maybe [ScopedName],Bool,ModuleName)] ->
    [(ScopedName,Maybe [ScopedName],Bool,ModuleName)]
filterByExportList modname exports names =
    [(phi name,map phi <$> children,False,modname) | (name,children,_,modname) <- names]
    where phi (ScopedName _ name) = ScopedName modname name

-- ToDo : write filter
filterByImportList :: Bool -> Maybe (Bool,[ImportEntity]) -> Maybe ModuleName ->
    [(ScopedName,Maybe [ScopedName],Bool,ModuleName)] ->
    [(ScopedName,Maybe [ScopedName],Bool,ModuleName)]
filterByImportList qualified imports alias names =
    [(phi name,map phi <$> children,qualified,modname) | (name,children,_,modname) <- names]
    where phi (ScopedName modname name) = case alias of
              Nothing -> ScopedName modname name
              Just alias' -> ScopedName alias' name

