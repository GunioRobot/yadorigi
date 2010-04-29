
module Yadorigi.SemanticAnalysis.Common where

import Data.List
import Data.Tuple.All
import Control.Arrow

import Yadorigi.Syntax

declToName :: Decl -> [String]
declToName (Decl _ _ decl) = primDeclToName decl

primDeclToName :: PrimDecl -> [String]
primDeclToName (BindPrimDecl (Bind lhs _) _) = lhsToOName lhs
primDeclToName _ = []

declToTypeName :: Decl -> [(String,[String])]
declToTypeName (Decl _ _ decl) = primDeclToTypeName decl

primDeclToTypeName :: PrimDecl -> [(String,[String])]
primDeclToTypeName (DataPrimDecl _ name _ body) = [(name,map fst body)]
primDeclToTypeName (TypePrimDecl name _ _) = [(name,[])]
primDeclToTypeName (ClassPrimDecl _ name _ body) = [(name,nub $ concatMap declToName body)]
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
primPatternToNames (DCPrimPattern _ pats) = concatMap patternToNames pats
primPatternToNames (LiteralPrimPattern _) = []
primPatternToNames (DCOpPrimPattern _ pat1 pat2) = patternToNames pat1++patternToNames pat2
primPatternToNames (NegativePrimPattern pat) = patternToNames pat
primPatternToNames (ListPrimPattern pats) = concatMap patternToNames pats
primPatternToNames (BindPrimPattern str pat) = str:maybe [] patternToNames pat
primPatternToNames (ParenthesesPrimPattern pat) = patternToNames pat
primPatternToNames (PrimPatternWithType pat _) = patternToNames pat

