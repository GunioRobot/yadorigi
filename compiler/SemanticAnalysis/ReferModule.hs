
module Yadorigi.SemanticAnalysis.ReferModule where

import Data.Functor
import Data.List
import Data.Maybe
import Data.Tuple.All

import Yadorigi.Common
import Yadorigi.Monad.Either
import Yadorigi.Syntax
import Yadorigi.SemanticAnalysis.Common

-- Data Types

data ImportError
    = ExportConflict ModuleName String [ModuleName]
    | ExportChildNotFound ModuleName NameWithModule [String]
    | ModuleNotFound Position ModuleName ModuleName
    | ManyModuleFound Position ModuleName ModuleName deriving Show

type NameWithModule = (ModuleName,String)
type NameInfo name = (name,ModuleName)
type TypeNameInfo name = (name,[String],ModuleName)
type NameEnv name = ([NameInfo name],[TypeNameInfo name])
type ModuleInfo = (ModuleName,Maybe [ExportEntity],[Import],NameEnv String,NameEnv NameWithModule)

-- refer module

referModule :: [Module] ->
    Either ImportError [(Module,ModuleName,[NameInfo NameWithModule],[TypeNameInfo NameWithModule])]
referModule modules =
    zipWith (\mod (modname,_,_,_,(names,types)) -> (mod,modname,names,types)) modules <$>
        (mapM genModuleInfo modules>>=iterateToConvergeM referModuleIter)

genModuleInfo :: Module -> Either ImportError ModuleInfo
genModuleInfo (Module modname exports imports body) = do
    insideNames <- return $ nub [((modname',name),modname) |
        name <- map (\(ScopedName _ _ str) -> str) $ concatMap declToName body,
        modname' <- [[],modname]]
    insideTypes <- return $ nub [((modname',name),children,modname) |
        (name,children) <- concatMap declToTypeName body, modname' <- [[],modname]]
    outsideNames <- filterByExportList modname exports insideNames
    outsideTypes <- typeFilterByExportList modname exports insideTypes
    return (modname,exports,imports,(outsideNames,outsideTypes),(insideNames,insideTypes))

-- ToDo : write import list checker
--        write export list checker

-- refer module iterator

referModuleIter :: [ModuleInfo] -> Either ImportError [ModuleInfo]
referModuleIter modules = mapM (referModuleIter' modules) modules

referModuleIter' :: [ModuleInfo] -> ModuleInfo -> Either ImportError ModuleInfo
referModuleIter' env (modname,exports,imports,_,(insideNames,insideTypes)) = do
    insideNames' <- (nub.(insideNames++).concat) <$> mapM (importModule env modname) imports
    insideTypes' <- (nub.(insideTypes++).concat) <$> mapM (importTypeModule env modname) imports
    outsideNames <- filterByExportList modname exports insideNames'
    outsideTypes <- typeFilterByExportList modname exports insideTypes'
    return (modname,exports,imports,(outsideNames,outsideTypes),(insideNames',insideTypes'))

importModule ::
    [ModuleInfo] -> ModuleName -> Import -> Either ImportError [NameInfo NameWithModule]
importModule env modname (Import pos qualified modname' alias imports) =
    case [mod | mod <- env, modname' == sel1 mod] of
        [mod] -> return $ filterByImportList imports
            ((if qualified then id else ([]:)) [fromMaybe modname' alias]) (fst $ sel4 mod)
        [] -> Left $ ModuleNotFound pos modname modname'
        _ -> Left $ ManyModuleFound pos modname modname'

importTypeModule ::
    [ModuleInfo] -> ModuleName -> Import -> Either ImportError [TypeNameInfo NameWithModule]
importTypeModule env modname (Import pos qualified modname' alias imports) =
    case [mod | mod <- env, modname' == sel1 mod] of
        [mod] -> return $ typeFilterByImportList imports
            ((if qualified then id else ([]:)) [fromMaybe modname' alias]) (snd $ sel4 mod)
        [] -> Left $ ModuleNotFound pos modname modname'
        _ -> Left $ ManyModuleFound pos modname modname'

-- Filter

filterByExportList :: ModuleName -> Maybe [ExportEntity] ->
    [NameInfo NameWithModule] -> Either ImportError [NameInfo String]
filterByExportList modname exports names =
    concat <$> mapM filterByExport (fromMaybe [ModuleExportEntity modname] exports)
    where
        filterByExport :: ExportEntity -> Either ImportError [NameInfo String]
        filterByExport (ModuleExportEntity modname') =
            return $ [(name,smodname) | ((modname'',name),smodname) <- names, modname' == modname'']
        filterByExport (NameExportEntity (ScopedName modname' _ name) (Just [])) =
            case nub [snd n | n <- names, elem modname' [fst $ fst n,[]], name == snd (fst n)] of
                [] -> return []
                [smodname] -> return [(name,smodname)]
                smodnames -> Left $ ExportConflict modname name smodnames
        filterByExport _ = return []

typeFilterByExportList :: ModuleName -> Maybe [ExportEntity] ->
    [TypeNameInfo NameWithModule] -> Either ImportError [TypeNameInfo String]
typeFilterByExportList modname exports names =
    concat <$> mapM filterByExport (fromMaybe [ModuleExportEntity modname] exports)
    where
        filterByExport :: ExportEntity -> Either ImportError [TypeNameInfo String]
        filterByExport (ModuleExportEntity modname') =
            return [(name,children,smodname) |
                ((modname'',name),children,smodname) <- names, modname' == modname'']
        filterByExport (NameExportEntity (ScopedName modname' _ name) exportChildren) =
            case nubBy (\(_,a) (_,b) -> a == b) [(sel2 n,sel3 n) |
                    n <- names, elem modname' [fst $ sel1 n,[]], name == snd (sel1 n)] of
                [] -> return []
                [(children,smodname)] -> do
                    let children' = fromMaybe [] (filter (flip notElem children) <$> exportChildren)
                    if null children'
                        then return [(name,fromMaybe children exportChildren,smodname)]
                        else Left $ ExportChildNotFound modname (modname',name) children'
                l -> Left $ ExportConflict modname name $ map snd l

-- ToDo : write filter
filterByImportList :: Maybe (Bool,[ImportEntity]) ->
    [ModuleName] -> [NameInfo String] -> [NameInfo NameWithModule]
filterByImportList Nothing modnames names =
    [((modname,name),smodname) | (name,smodname) <- names, modname <- modnames]
filterByImportList (Just (False,imports)) modnames names = writeLater
filterByImportList (Just (True,imports)) modnames names = writeLater

typeFilterByImportList :: Maybe (Bool,[ImportEntity]) ->
    [ModuleName] -> [TypeNameInfo String] -> [TypeNameInfo NameWithModule]
typeFilterByImportList Nothing modnames names =
    [((modname,name),children,smodname) | (name,children,smodname) <- names, modname <- modnames]
typeFilterByImportList (Just (False,imports)) modnames names = writeLater
typeFilterByImportList (Just (True,imports)) modnames names = writeLater

