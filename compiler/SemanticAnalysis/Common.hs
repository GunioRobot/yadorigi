
module Yadorigi.SemanticAnalysis.Common where

import Data.List

import Yadorigi.Syntax

declToName :: Decl -> [String]
declToName (Decl _ _ decl) = primDeclToName decl

primDeclToName :: PrimDecl -> [String]
primDeclToName (BindDecl (Bind lhs _) _) = lhsToOName lhs
primDeclToName _ = []

declToTypeName :: Decl -> [(String,[String])]
declToTypeName (Decl _ _ decl) = primDeclToTypeName decl

primDeclToTypeName :: PrimDecl -> [(String,[String])]
primDeclToTypeName (DataDecl _ name _ body) = [(name,map fst body)]
primDeclToTypeName (TypeDecl name _ _) = [(name,[])]
primDeclToTypeName (ClassDecl _ name _ body) = [(name,nub $ concatMap declToName body)]
primDeclToTypeName _ = []

lhsToOName :: Lhs -> [String]
lhsToOName (FunctionLhs name _) = [name]
lhsToOName (InfixLhs name _ _) = [name]
lhsToOName (PatternLhs pat) = patternToNames pat

lhsToIName :: Lhs -> [String]
lhsToIName (FunctionLhs _ pat) = nub $ concatMap patternToNames pat
lhsToIName (InfixLhs _ pat1 pat2) = nub $ patternToNames pat1++patternToNames pat2
lhsToIName (PatternLhs _) = []

patternToNames :: PatternMatch -> [String]
patternToNames (PatternMatch _ pat) = primPatternToNames pat

primPatternToNames :: PrimPatternMatch -> [String]
primPatternToNames (DCPattern _ pats) = concatMap patternToNames pats
--primPatternToNames (LiteralPattern _) = []
primPatternToNames (DCOpPattern _ pat1 pat2) = patternToNames pat1++patternToNames pat2
primPatternToNames (NegativePattern pat) = patternToNames pat
primPatternToNames (ListPattern pats) = concatMap patternToNames pats
primPatternToNames (BindPattern str pat) = str:maybe [] patternToNames pat
primPatternToNames (ParenthesesPattern pat) = patternToNames pat
primPatternToNames (PatternWithType pat _) = patternToNames pat
--primPatternToNames WildCardPattern = []
primPatternToNames _ = []
