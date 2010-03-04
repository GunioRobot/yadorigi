
module Yadorigi.SemanticAnalysis.ScopeResolution where

import Data.List
import Control.Arrow

import Yadorigi.Parser.DataTypes

-- Bind Scope Name To Pattern Bind

bindScopeNameToPrimDecl :: Int -> PrimDecl -> (Int,PrimDecl)
bindScopeNameToPrimDecl n (PrimBindDecl (Bind (PatternLhs "" pat) rhs) decls) = (n+1,
    PrimBindDecl (Bind (PatternLhs ("\\pattern"++show n) pat) rhs) (bindScopeNameToDecls decls))
bindScopeNameToPrimDecl n d = (n,d)

bindScopeNameToDecls :: [Decl] -> [Decl]
bindScopeNameToDecls =
    snd.mapAccumL (\n (Decl pos d) -> second (Decl pos) $ bindScopeNameToPrimDecl n d) 0

bindScopeNameToTopDecls :: [TopDecl] -> [TopDecl]
bindScopeNameToTopDecls = snd.mapAccumL iter 0
    where iter n (TopDecl pos (PrimDeclDecl d)) =
              second (TopDecl pos.PrimDeclDecl) $ bindScopeNameToPrimDecl n d
          iter n d = (n,d)

bindScopeNameToModule :: Module -> Module
bindScopeNameToModule (Module name imports decls) =
    Module name imports (bindScopeNameToTopDecls decls)

-- Alpha Conversion

--alpha :: ModuleName -> Decl -> Decl
--alpha modname (Decl pos (PrimBindDecl Bind [Decl])) =
--    Decl pos (PrimBindDecl (alpha' modname Bind) ())
--    where alpha' 
--alpha modname decl = decl

-- Get Module Interface

--getModuleInterface :: Module -> ([String],[String])
--getModuleInterface mod = undefined

