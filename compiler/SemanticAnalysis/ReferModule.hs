
module Yadorigi.SemanticAnalysis.ReferModule where

import Data.Functor
import Data.List
import Data.Maybe
import Data.Tuple.All
import Control.Arrow

import Yadorigi.Common
import Yadorigi.Monad.Either
import Yadorigi.Syntax
import Yadorigi.SemanticAnalysis.Common

-- Data Types

data ImportError
    = ExportConflict ModuleName String [ModuleName]
    | ModuleNotFound Position ModuleName ModuleName
    | ManyModuleFound Position ModuleName ModuleName deriving Show

type NameInfo = (ScopedName,[String],ModuleName)
type ModuleInfo = (ModuleName,Maybe [ExportEntity],[Import],[NameInfo],[NameInfo])

-- refer module

referModule :: [Module] -> Either ImportError [(ModuleName,[NameInfo])]
referModule modules =
    map (sel1&&&sel5) <$> iterateToConvergeM referModuleIter (map genModuleInfo modules)

genModuleInfo :: Module -> ModuleInfo
genModuleInfo (Module modname exports imports body) =
    let insideNames = nub
            [(ScopedName modname' [] name,children,modname) |
                (name,children) <- concatMap declToName body,modname' <- [[],modname]]
        outsideNames = filterByExportList modname exports insideNames in
            (modname,exports,imports,outsideNames,insideNames)

-- ToDo : write import list checker
--        write export list checker

-- refer module iterator

referModuleIter :: [ModuleInfo] -> Either ImportError [ModuleInfo]
referModuleIter modules = mapM (referModuleIter' modules) modules

referModuleIter' :: [ModuleInfo] -> ModuleInfo -> Either ImportError ModuleInfo
referModuleIter' env (modname,exports,imports,outsideNames,insideNames) = do
    insideNames' <- (nub.(insideNames++).concat) <$> mapM (importModule env modname) imports
    let outsideNames' = filterByExportList modname exports insideNames'
    return (modname,exports,imports,outsideNames',insideNames')

importModule :: [ModuleInfo] -> ModuleName -> Import -> Either ImportError [NameInfo]
importModule env modname (Import pos qualified modname' alias imports) =
    case [mod | mod <- env, modname' == sel1 mod] of
        [mod] -> return $ filterByImportList imports
            ((if qualified then id else ([]:)) [fromMaybe modname' alias]) (sel4 mod)
        [] -> Left $ ModuleNotFound pos modname modname'
        _ -> Left $ ManyModuleFound pos modname modname'

filterByExportList :: ModuleName -> Maybe [ExportEntity] -> [NameInfo] -> [NameInfo]
filterByExportList modname exports names = nub
    [(ScopedName [] [] name,children,smodname) |
        export <- fromMaybe [ModuleExportEntity modname] exports,
        (ScopedName _ _ name,children,smodname) <- catMaybes $ map (exportFilter export) names]

exportFilter :: ExportEntity -> NameInfo -> Maybe NameInfo
exportFilter (ModuleExportEntity modname) entity@(ScopedName modname' _ _,_,_)
    | modname == modname' = Just entity
exportFilter (NameExportEntity (ScopedName modname _ name) childrenExports)
    (ScopedName modname' _ name',children,modname'')
    | name == name' && (null modname || modname == modname') =
        Just (ScopedName modname' [] name',filteredChildren,modname'')
    where filteredChildren = nub [name | export <- fromMaybe children childrenExports,
              name <- children, export == name]
exportFilter _ _ = Nothing

-- ToDo : write filter
filterByImportList ::
    Maybe (Bool,[ImportEntity]) -> [ModuleName] -> [NameInfo] -> [NameInfo]
filterByImportList Nothing modnames names =
    [(ScopedName modname [] name,children,smodname) |
        (ScopedName _ _ name,children,smodname) <- names,
        modname <- modnames]
filterByImportList (Just (False,imports)) modnames names = writeLater

