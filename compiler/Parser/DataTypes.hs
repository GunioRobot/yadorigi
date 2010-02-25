
module Yadorigi.Parser.DataTypes where

import Text.Parsec

import Data.List

-- Position, Layout

data Position = Position Int Int deriving Show

type LayoutInfo = Either Int Int

-- Name

type ModuleName = [String]

data ScopedName = ScopedName ModuleName String deriving Eq

instance Show ScopedName where
    show (ScopedName scope name) = concatMap (++".") scope++name

-- Literal

data Literal
    = LiteralInt Int {- integer literal -}
    | LiteralFloat Float {- floating point number literal -}
    | LiteralChar Char {- character literal -}
    | LiteralString String {- character string literal -}
        deriving Eq

instance Show Literal where
    show (LiteralInt i) = show i
    show (LiteralFloat f) = show f
    show (LiteralChar c) = show c
    show (LiteralString s) = show s

-- Token

data Token' = Token' SourcePos Token

instance Show Token' where
    show (Token' pos t) = show pos++" "++show t

type TokenStream = [Token']

data Token
    = LiteralToken Literal {- literal token -}
    | NameToken ScopedName {- name token -}
    | OpToken ScopedName {- operator token -}
    | ReservedToken String {- reserved word token, reserved symbol token -}
        deriving (Eq,Show)

-- Data Type

data DataTypeWithContext = DataTypeWithContext Position [TypeContext] DataType

instance Show DataTypeWithContext where
    show (DataTypeWithContext _ [] dataType) = show dataType
    show (DataTypeWithContext _ typeContext dataType) =
        "("++(intercalate "," $ map show typeContext)++") => "++show dataType

data TypeContext = TypeContext Position ScopedName DataType

instance Show TypeContext where
    show (TypeContext _ typeClass typeName) = show typeClass++" "++show typeName

data DataType = DataType Position PrimDataType

instance Show DataType where
    show (DataType _ t) = show t

data PrimDataType
    = PrimVariableType String {- variable type -}
    | PrimConstructorType ScopedName {- constructor type -}
    | PrimReservedConstructorType String {- reserved constructor type -}
    | PrimComposedType DataType DataType {- composed type -}
    | PrimListType DataType {- list type -}
    | PrimFunctionType DataType DataType {- function type -}
    | PrimParenthesesType DataType {- parentheses type -}

instance Show PrimDataType where
    show (PrimVariableType str) = str
    show (PrimConstructorType name) = show name
    show (PrimReservedConstructorType str) = str
    show (PrimComposedType cons param) = "("++show cons++" "++show param++")"
    show (PrimListType param) = "["++show param++"]"
    show (PrimFunctionType t1 t2) = "("++show t1++" -> "++show t2++")"
    show (PrimParenthesesType t) = "("++show t++")"

-- Pattern Match

data PatternMatch = PatternMatch Position PrimPatternMatch

instance Show PatternMatch where
    show (PatternMatch pos pattern) = show pattern

data PrimPatternMatch
    = DCPrimPattern ScopedName [PatternMatch] {- data constructor pattern -}
    | LiteralPrimPattern Literal {- literal pattern -}
    | DCOpPrimPattern ScopedName PatternMatch PatternMatch {- infix data constructor pattern -}
    | NegativePrimPattern PatternMatch {- negative pattern -}
    | ListPrimPattern [PatternMatch] {- list pattern -}
    | BindPrimPattern String (Maybe PatternMatch) {- bind pattern, wild card pattern, as pattern -}
    | ParenthesesPrimPattern PatternMatch {- Parentheses Pattern -}
    | PrimPatternWithType PatternMatch DataTypeWithContext {- pattern with data type information -}

instance Show PrimPatternMatch where
    show (DCPrimPattern name list) = "("++show name++concatMap ((' ':).show) list++")"
    show (LiteralPrimPattern literal) = show literal
    show (DCOpPrimPattern name expr1 expr2) = "("++show expr1++" "++show name++" "++show expr2++")"
    show (ListPrimPattern list) = show list
    show (BindPrimPattern str Nothing) = str
    show (BindPrimPattern str (Just pattern)) = "("++str++"@"++show pattern++")"
    show (ParenthesesPrimPattern pattern) = "("++show pattern++")"
    show (PrimPatternWithType pattern typeName) = "("++show pattern++show typeName++")"

-- Expression

data Expr = Expr Position PrimExpr

instance Show Expr where
    show (Expr pos primExpr) = show primExpr

data PrimExpr
    = LiteralPrimExpr Literal {- literal expression -}
    | NamePrimExpr ScopedName {- name expression -}
    | ApplyFunctionPrimExpr Expr Expr {- apply function expression -}
    | InfixPrimExpr ScopedName Expr Expr {- infix expression -}
    | NegativePrimExpr Expr {- negative expression -}
    | ParenthesesPrimExpr Expr {- parentheses expression -}
    | ListPrimExpr [Expr] {- list expression -}
    | LambdaPrimExpr [Lambda] {- lambda expression -}
    | LetPrimExpr [PrimLet] Expr {- let expression -}
    | IfPrimExpr Expr Expr Expr {- if expression -}
    | CasePrimExpr Expr [CasePattern] {- case Expression -}
    | PrimExprWithType Expr DataTypeWithContext {- expression with data type information -}

instance Show PrimExpr where
    show (LiteralPrimExpr literal) = show literal
    show (NamePrimExpr name) = show name
    show (ApplyFunctionPrimExpr func param) = "("++show func++" "++show param++")"
    show (InfixPrimExpr name expr1 expr2) = "("++show expr1++" "++show name++" "++show expr2++")"
    show (NegativePrimExpr expr) = "-"++show expr
    show (ParenthesesPrimExpr expr) = "("++show expr++")"
    show (ListPrimExpr list) = show list
    show (LambdaPrimExpr list) = "(\\"++(intercalate " | " $ map show list)++")"
    show (LetPrimExpr list expr) = "(let "++show list++" "++show expr++")"
    show (IfPrimExpr c t f) = "(if "++show c++" "++show t++" "++show f++")"
    show (CasePrimExpr expr list) = "(case "++show expr++" "++show list++")"
    show (PrimExprWithType expr dataType) = "("++show expr++"::"++show dataType++")"

data Guard = Guard Expr Expr

instance Show Guard where
    show (Guard cond expr) = "| "++show cond++" "++show expr

type ExprOrGuard = Either Expr [Guard]

data Lambda = Lambda Position [PatternMatch] Expr

instance Show Lambda where
    show (Lambda pos params expr) = (intercalate " " $ map show params)++" -> "++show expr

data PrimLet = PrimLet BindLhs ExprOrGuard

instance Show PrimLet where
    show (PrimLet lhs expr) = show lhs++" = "++either show show expr

data CasePattern = CasePattern PatternMatch ExprOrGuard

instance Show CasePattern where
    show (CasePattern pattern expr) = show pattern++" "++either show show expr

-- Declaration

data TopDecl = TopDecl Position PrimTopDecl

instance Show TopDecl where
    show (TopDecl _ decl) = show decl

data PrimTopDecl
    = PrimDataDecl [TypeContext] String [String] [(String,[DataType])]
    | PrimTypeDecl String [String] DataTypeWithContext
    | PrimClassDecl [TypeContext] String String [Decl]
    | PrimInstanceDecl [TypeContext] String DataType [Decl]
    | PrimDeclDecl PrimDecl

instance Show PrimTopDecl where
    show (PrimDataDecl [] str param body) = "data "++str++concatMap (' ':) param++" = "++
        intercalate " | " (map (\(s,l) -> s++concatMap ((' ':).show) l) body)
    show (PrimDataDecl context str param body) =
        "data "++show context++" => "++str++concatMap (' ':) param++" = "++
        intercalate " | " (map (\(s,l) -> s++concatMap ((' ':).show) l) body)
    show (PrimTypeDecl str param typeName) =
        "type "++str++intercalate " " param++" = "++show typeName
    show (PrimClassDecl [] className typeName body) =
        "class "++className++" "++typeName++showWhereClause body
    show (PrimClassDecl context className typeName body) =
        "class "++show context++" => "++className++" "++typeName++showWhereClause body
    show (PrimInstanceDecl [] className typeName body) =
        "instance "++className++" "++show typeName++showWhereClause body
    show (PrimInstanceDecl context className typeName body) =
        "instance "++show context++" => "++className++" "++show typeName++showWhereClause body
    show (PrimDeclDecl decl) = show decl

data Decl = Decl Position PrimDecl

instance Show Decl where
    show (Decl pos decl) = show decl

data PrimDecl
    = PrimFixityDecl Fixity (Maybe Int) [ScopedName]
    | PrimTypeSignature String DataTypeWithContext
    | PrimBindDecl BindLhs ExprOrGuard [Decl]

instance Show PrimDecl where
    show (PrimFixityDecl fixity Nothing list) = show fixity++" "++show list
    show (PrimFixityDecl fixity (Just num) list) = show fixity++" "++show num++" "++show list
    show (PrimTypeSignature str typeName) = str++" :: "++show typeName
    show (PrimBindDecl lhs body whereClause) =
        show lhs++" = "++either show show body++showWhereClause whereClause

data Fixity = Infixl | Infix | Infixr deriving Show

data BindLhs
    = FunctionBindLhs String [PatternMatch]
    | InfixBindLhs String PatternMatch PatternMatch
    | PatternBindLhs PatternMatch

instance Show BindLhs where
    show (FunctionBindLhs name params) = name++concatMap ((' ':).show) params
    show (InfixBindLhs name pat1 pat2) = show pat1++" "++name++" "++show pat2
    show (PatternBindLhs pat) = show pat

-- Module

data Module
    = Module ModuleName [ModuleName] [TopDecl]

instance Show Module where
    show (Module [] imports decls) = concatMap ((++"\n").("import "++).intercalate ".") imports++
        "\n"++concatMap ((++"\n").show) decls
    show (Module modname imports decls) = "module "++intercalate "." modname++" where\n"++
        concatMap ((++"\n").("import "++).intercalate ".") imports++
        "\n"++concatMap ((++"\n").show) decls

-- Output Functions

showWhereClause :: (Show a) => [a] -> String
showWhereClause [] = ""
showWhereClause list = "where\n"++showLayoutList list

showLayoutList :: (Show a) => [a] -> String
showLayoutList = unlines.map ("    "++).lines.concatMap show

-- Composed Data Constructors

dcPattern :: Position -> ScopedName -> [PatternMatch] -> PatternMatch
dcPattern pos name = PatternMatch pos.DCPrimPattern name

literalPattern :: Position -> Literal -> PatternMatch
literalPattern pos = PatternMatch pos.LiteralPrimPattern

dcOpPattern :: Position -> ScopedName -> PatternMatch -> PatternMatch -> PatternMatch
dcOpPattern pos name pat = PatternMatch pos.DCOpPrimPattern name pat

negativePattern :: Position -> PatternMatch -> PatternMatch
negativePattern pos = PatternMatch pos.NegativePrimPattern

listPattern :: Position -> [PatternMatch] -> PatternMatch
listPattern pos = PatternMatch pos.ListPrimPattern

bindPattern :: Position -> String -> PatternMatch
bindPattern pos str = PatternMatch pos $ BindPrimPattern str Nothing

asPattern :: Position -> String -> PatternMatch -> PatternMatch
asPattern pos str = PatternMatch pos.BindPrimPattern str.Just

parenthesesPattern :: Position -> PatternMatch -> PatternMatch
parenthesesPattern pos = PatternMatch pos.ParenthesesPrimPattern

patternWithType :: Position -> PatternMatch -> DataTypeWithContext -> PatternMatch
patternWithType pos pat = PatternMatch pos.PrimPatternWithType pat


literalExpr :: Position -> Literal -> Expr
literalExpr pos = Expr pos.LiteralPrimExpr

nameExpr :: Position -> ScopedName -> Expr
nameExpr pos = Expr pos.NamePrimExpr

applyFunctionExpr :: Position -> Expr -> Expr -> Expr
applyFunctionExpr pos func = Expr pos.ApplyFunctionPrimExpr func

infixExpr :: Position -> ScopedName -> Expr -> Expr -> Expr
infixExpr pos name expr = Expr pos.InfixPrimExpr name expr

negativeExpr :: Position -> Expr -> Expr
negativeExpr pos = Expr pos.NegativePrimExpr

parenthesesExpr :: Position -> Expr -> Expr
parenthesesExpr pos = Expr pos.ParenthesesPrimExpr

listExpr :: Position -> [Expr] -> Expr
listExpr pos = Expr pos.ListPrimExpr

lambdaExpr :: Position -> [Lambda] -> Expr
lambdaExpr pos = Expr pos.LambdaPrimExpr

letExpr :: Position -> [PrimLet] -> Expr -> Expr
letExpr pos list = Expr pos.LetPrimExpr list

ifExpr :: Position -> Expr -> Expr -> Expr -> Expr
ifExpr pos c t = Expr pos.IfPrimExpr c t

caseExpr :: Position -> Expr -> [CasePattern] -> Expr
caseExpr pos expr = Expr pos.CasePrimExpr expr

exprWithType :: Position -> Expr -> DataTypeWithContext -> Expr
exprWithType pos expr = Expr pos.PrimExprWithType expr


variableType :: Position -> String -> DataType
variableType pos = DataType pos.PrimVariableType

constructorType :: Position -> ScopedName -> DataType
constructorType pos = DataType pos.PrimConstructorType

reservedConstructorType :: Position -> String -> DataType
reservedConstructorType pos = DataType pos.PrimReservedConstructorType

composedType :: Position -> DataType -> DataType -> DataType
composedType pos cons = DataType pos.PrimComposedType cons

listType :: Position -> DataType -> DataType
listType pos = DataType pos.PrimListType

functionType :: Position -> DataType -> DataType -> DataType
functionType pos f = DataType pos.PrimFunctionType f

parenthesesType :: Position -> DataType -> DataType
parenthesesType pos = DataType pos.PrimParenthesesType


dataDecl :: Position -> [TypeContext] -> String -> [String] -> [(String,[DataType])] -> TopDecl
dataDecl pos context typeName params = TopDecl pos.PrimDataDecl context typeName params

typeDecl :: Position -> String -> [String] -> DataTypeWithContext -> TopDecl
typeDecl pos typeName params = TopDecl pos.PrimTypeDecl typeName params

classDecl :: Position -> [TypeContext] -> String -> String -> [Decl] -> TopDecl
classDecl pos context className typeVar = TopDecl pos.PrimClassDecl context className typeVar

instanceDecl :: Position -> [TypeContext] -> String -> DataType -> [Decl] -> TopDecl
instanceDecl pos context className typeName =
    TopDecl pos.PrimInstanceDecl context className typeName


declToTopDecl :: Decl -> TopDecl
declToTopDecl (Decl pos decl) = TopDecl pos $ PrimDeclDecl decl


fixityDecl :: Position -> Fixity -> (Maybe Int) -> [ScopedName] -> Decl
fixityDecl pos fixity level = Decl pos.PrimFixityDecl fixity level

typeSignatureDecl :: Position -> String -> DataTypeWithContext -> Decl
typeSignatureDecl pos name = Decl pos.PrimTypeSignature name

bindDecl :: Position -> BindLhs -> ExprOrGuard -> [Decl] -> Decl
bindDecl pos lhs body = Decl pos.PrimBindDecl lhs body

