
module Yadorigi.Parser.DataTypes where

-- Data Types

data Position = Position Int Int

data PlusPos body = PlusPos Position body

data LayoutInfo = LayoutInfo Bool Int


data Literal = LiteralInt Int | LiteralFloat Float | LiteralChar Char | LiteralString String

data PatternMatch = PatternMatch Position PrimPatternMatch

data PrimPatternMatch
    = DCPrimPattern String [PatternMatch] {- data constructor pattern -}
    | LiteralPrimPattern Literal {- literal pattern -}
    | DCOpPrimPattern String PatternMatch PatternMatch {- infix data constructor pattern -}
    | ListPrimPattern [PatternMatch] {- list pattern -}
    | BindPrimPattern String (Maybe PatternMatch)
        {- bind pattern (including wild card pattern and as pattern) -}
    | BracketPrimPattern PatternMatch {- Bracket Pattern -}

data LetOne = LetOne PatternMatch Expr

data CaseGuard = CaseGuard Expr Expr

data CasePattern = CasePattern PatternMatch (Either Expr [CaseGuard])

data Expr = Expr Position PrimExpr

data PrimExpr
    = LiteralPrimExpr Literal {- literal expression -}
    | NamePrimExpr String {- name expression -}
    | ApplyFunctionPrimExpr Expr Expr {- apply function expression -}
    | InfixPrimExpr String Expr Expr {- infix expression -}
    | NegativePrimExpr Expr {- negative expression -}
    | BracketPrimExpr Expr {- bracket expression -}
    | ListPrimExpr [Expr] {- list expression -}
    | LetPrimExpr [LetOne] Expr {- let expression -}
    | IfPrimExpr Expr Expr Expr {- if expression -}
    | CasePrimExpr Expr [CasePattern] {- case Expression -}

-- Output Format

instance (Show body) => Show (PlusPos body) where
    show (PlusPos (Position line column) body) = show line++","++show column++" "++show body

instance Show Literal where
    show (LiteralInt i) = show i
    show (LiteralFloat f) = show f
    show (LiteralChar c) = show c
    show (LiteralString s) = show s

instance Show PatternMatch where
    show (PatternMatch pos pattern) = show pattern

instance Show PrimPatternMatch where
    show (DCPrimPattern str list) = str++concatMap ((' ':).show) list
    show (LiteralPrimPattern literal) = show literal
    show (DCOpPrimPattern str expr1 expr2) = "{"++str++" "++show expr1++" "++show expr2++"}"
    show (ListPrimPattern list) = show list
    show (BindPrimPattern str Nothing) = str
    show (BindPrimPattern str (Just pattern)) = str++"@"++show pattern
    show (BracketPrimPattern pattern) = "("++show pattern++")"

instance Show LetOne where
    show (LetOne pattern expr) = show pattern++" = "++show expr

instance Show CaseGuard where
    show (CaseGuard cond expr) = "| "++show cond++" "++show expr

instance Show CasePattern where
    show (CasePattern pattern (Left expr)) = show pattern++" "++show expr
    show (CasePattern pattern (Right list)) = show pattern++" "++show list

instance Show Expr where
    show (Expr pos primExpr) = show primExpr

instance Show PrimExpr where
    show (LiteralPrimExpr literal) = show literal
    show (NamePrimExpr name) = name
    show (ApplyFunctionPrimExpr func param) = show func++" "++show param
    show (InfixPrimExpr str expr1 expr2) = "{"++str++" "++show expr1++" "++show expr2++"}"
    show (NegativePrimExpr expr) = "-"++show expr
    show (BracketPrimExpr expr) = "("++show expr++")"
    show (ListPrimExpr list) = show list
    show (LetPrimExpr list expr) = "{let "++show list++" "++show expr++"}"
    show (IfPrimExpr c t f) = "{if "++show c++" "++show t++" "++show f++"}"
    show (CasePrimExpr expr list) = "{case "++show expr++" "++show list++"}"

-- Composed Data Constructors

literalExpr :: Position -> Literal -> Expr
literalExpr pos = Expr pos.LiteralPrimExpr

nameExpr :: Position -> String -> Expr
nameExpr pos = Expr pos.NamePrimExpr

applyFunctionExpr :: Position -> Expr -> Expr -> Expr
applyFunctionExpr pos func = Expr pos.ApplyFunctionPrimExpr func

infixExpr :: Position -> String -> Expr -> Expr -> Expr
infixExpr pos str expr = Expr pos.InfixPrimExpr str expr

negativeExpr :: Position -> Expr -> Expr
negativeExpr pos = Expr pos.NegativePrimExpr

bracketExpr :: Position -> Expr -> Expr
bracketExpr pos = Expr pos.BracketPrimExpr

listExpr :: Position -> [Expr] -> Expr
listExpr pos = Expr pos.ListPrimExpr

letExpr :: Position -> [LetOne] -> Expr -> Expr
letExpr pos list = Expr pos.LetPrimExpr list

ifExpr :: Position -> Expr -> Expr -> Expr -> Expr
ifExpr pos c t = Expr pos.IfPrimExpr c t

caseExpr :: Position -> Expr -> [CasePattern] -> Expr
caseExpr pos expr = Expr pos.CasePrimExpr expr


dcPattern :: Position -> String -> [PatternMatch] -> PatternMatch
dcPattern pos str = PatternMatch pos.DCPrimPattern str

literalPattern :: Position -> Literal -> PatternMatch
literalPattern pos = PatternMatch pos.LiteralPrimPattern

dcOpPattern :: Position -> String -> PatternMatch -> PatternMatch -> PatternMatch
dcOpPattern pos str pat = PatternMatch pos.DCOpPrimPattern str pat

listPattern :: Position -> [PatternMatch] -> PatternMatch
listPattern pos = PatternMatch pos.ListPrimPattern

bindPattern :: Position -> String -> PatternMatch
bindPattern pos str = PatternMatch pos $ BindPrimPattern str Nothing

asPattern :: Position -> String -> PatternMatch -> PatternMatch
asPattern pos str = PatternMatch pos.BindPrimPattern str.Just

bracketPattern :: Position -> PatternMatch -> PatternMatch
bracketPattern pos = PatternMatch pos.BracketPrimPattern
