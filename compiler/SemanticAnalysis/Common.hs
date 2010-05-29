
module Yadorigi.SemanticAnalysis.Common where

import Data.List

import Yadorigi.Syntax

declToName :: Decl -> [ScopedName]
declToName (Decl _ decl) = primDeclToName decl

primDeclToName :: PrimDecl -> [ScopedName]
primDeclToName (BindDecl _ (Bind lhs _) _) = lhsToOName lhs
primDeclToName _ = []

declToTypeName :: Decl -> [(String,[String])]
declToTypeName (Decl _ decl) = primDeclToTypeName decl

primDeclToTypeName :: PrimDecl -> [(String,[String])]
primDeclToTypeName (DataDecl _ (ScopedName _ _ name) _ body) =
    [(name,[child | (ScopedName _ _ child,_) <- body])]
primDeclToTypeName (TypeDecl (ScopedName _ _ name) _ _) = [(name,[])]
primDeclToTypeName (ClassDecl _ (ScopedName _ _ name) _ body) =
    [(name,nub $ map (\(ScopedName _ _ str) -> str) $ concatMap declToName body)]
primDeclToTypeName _ = []

lhsToOName :: Lhs -> [ScopedName]
lhsToOName (FunctionLhs name _) = [name]
lhsToOName (InfixLhs name _ _) = [name]
lhsToOName (PatternLhs pat) = patternToNames pat

lhsToIName :: Lhs -> [ScopedName]
lhsToIName (FunctionLhs _ pat) = nub $ concatMap patternToNames pat
lhsToIName (InfixLhs _ pat1 pat2) = nub $ patternToNames pat1++patternToNames pat2
lhsToIName (PatternLhs _) = []

patternToNames :: PatternMatch -> [ScopedName]
patternToNames (PatternMatch _ pat) = primPatternToNames pat

primPatternToNames :: PrimPatternMatch -> [ScopedName]
primPatternToNames (DCPattern _ pats) = concatMap patternToNames pats
--primPatternToNames (LiteralPattern _) = []
primPatternToNames (DCOpPattern _ pat1 pat2) = patternToNames pat1++patternToNames pat2
primPatternToNames (NegativePattern pat) = patternToNames pat
primPatternToNames (ListPattern pats) = concatMap patternToNames pats
primPatternToNames (BindPattern name pat) = name:maybe [] patternToNames pat
primPatternToNames (ParenthesesPattern pat) = patternToNames pat
primPatternToNames (TypeSignaturePattern pat _) = patternToNames pat
--primPatternToNames WildCardPattern = []
primPatternToNames _ = []

