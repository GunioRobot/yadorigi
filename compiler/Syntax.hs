
module Yadorigi.Syntax where

import Data.List

-- Module

data Module = Module ModuleName (Maybe [ExportEntity]) [Import] [Decl]

instance Show Module where
    show (Module [] _ imports decls) =
        unlines (map show imports)++"\n"++concatMap ((++"\n").show) decls
    show (Module modname exports imports decls) =
        "module "++showModuleName modname++
        " ("++maybe ".." (intercalate ",".map show) exports++") where\n"++
        unlines (map show imports)++"\n"++concatMap ((++"\n").show) decls

data Import = Import Position Bool ModuleName (Maybe ModuleName) (Maybe (Bool,[ImportEntity]))
    deriving Eq

instance Show Import where
    show (Import _ qualified modname alias Nothing) =
        "import "++(if qualified then " qualified" else "")++showModuleName modname++
        (maybe "" ((" as "++).showModuleName) alias)
    show (Import _ qualified modname alias (Just (hiding,imports))) =
        "import "++(if qualified then " qualified" else "")++showModuleName modname++
        (maybe "" ((" as "++).showModuleName) alias)++
        (if hiding then "hiding" else "")++" ("++intercalate "," (map show imports)++")"

data ExportEntity
    = ModuleExportEntity ModuleName
    | NameExportEntity ScopedName (Maybe [String]) deriving Eq

instance Show ExportEntity where
    show (ModuleExportEntity modname) = "module "++show modname
    show (NameExportEntity name Nothing) = show name++" (..)"
    show (NameExportEntity name (Just [])) = show name
    show (NameExportEntity name (Just children)) =
        show name++" ("++intercalate " , " children++")"

data ImportEntity = ImportEntity String (Maybe [String]) deriving Eq

instance Show ImportEntity where
    show (ImportEntity name Nothing) = name++" (..)"
    show (ImportEntity name (Just children)) =
        show name++" ("++intercalate " , " children++")"

-- Declaration

data Decl = Decl Position Int PrimDecl

instance Show Decl where
    show (Decl _ scope decl) = "#"++show scope++"# "++show decl

data PrimDecl
    = DataDecl [TypeContext] String [(String,Int)] [(String,[DataType])]
    | TypeDecl String [(String,Int)] DataTypeWithContext
    | ClassDecl [TypeContext] String (String,Int) [Decl]
    | InstanceDecl [TypeContext] ScopedName DataType [Decl]
    | FixityDecl Fixity (Maybe Int) [String]
    | TypeSignatureDecl String DataTypeWithContext
    | BindDecl Bind [Decl]

instance Show PrimDecl where
    show (DataDecl context str param body) =
        "data "++showContext context++str++concatMap ((' ':).fst) param++" = "++
        intercalate " | " (map (\(s,l) -> s++concatMap ((' ':).show) l) body)
    show (TypeDecl str param typeName) =
        "type "++str++intercalate " " (map fst param)++" = "++show typeName
    show (ClassDecl context className typeName body) =
        "class "++showContext context++className++" "++fst typeName++showWhereClause body
    show (InstanceDecl context className typeName body) =
        "instance "++showContext context++show className++" "++show typeName++showWhereClause body
    show (FixityDecl fixity Nothing list) = show fixity++" "++show list
    show (FixityDecl fixity (Just num) list) = show fixity++" "++show num++" "++intercalate " " list
    show (TypeSignatureDecl str typeName) = str++" :: "++show typeName
    show (BindDecl bind whereClause) = show bind++showWhereClause whereClause

data Fixity = Infixl | Infix | Infixr deriving Show

-- Lhs, Rhs, Bind

data Bind = Bind Lhs Rhs

instance Show Bind where
    show (Bind lhs rhs) = show lhs++" = "++show rhs

data Lhs
    = FunctionLhs String [PatternMatch]
    | InfixLhs String PatternMatch PatternMatch
    | PatternLhs PatternMatch

instance Show Lhs where
    show (FunctionLhs name param) = name++concatMap ((' ':).show) param
    show (InfixLhs name pat1 pat2) = show pat1++" "++name++" "++show pat2
    show (PatternLhs pat) = show pat

data Rhs
    = ExprRhs Expr
    | GuardRhs [Guard]

instance Show Rhs where
    show (ExprRhs expr) = show expr
    show (GuardRhs guard) = show guard

data Guard = Guard Expr Expr

instance Show Guard where
    show (Guard cond expr) = "| "++show cond++" "++show expr

-- Expression

data Expr = Expr Position PrimExpr

instance Show Expr where
    show (Expr _ primExpr) = show primExpr

data PrimExpr
    = LiteralExpr Literal {- literal expression -}
    | NameExpr ScopedName {- name expression -}
    | ApplyExpr Expr Expr {- apply function expression -}
    | InfixExpr ScopedName Expr Expr {- infix expression -}
    | NegativeExpr Expr {- negative expression -}
    | ParenthesesExpr Expr {- parentheses expression -}
    | ListExpr [Expr] {- list expression -}
    | LambdaExpr [Lambda] {- lambda expression -}
    | LetExpr Int [(Position,PrimDecl)] Expr {- let expression -}
    | IfExpr Expr Expr Expr {- if expression -}
    | CaseExpr Expr [CasePattern] {- case Expression -}
    | TypeSignatureExpr Expr DataTypeWithContext {- expression with data type information -}

instance Show PrimExpr where
    show (LiteralExpr literal) = show literal
    show (NameExpr name) = show name
    show (ApplyExpr func param) = "("++show func++" "++show param++")"
    show (InfixExpr name expr1 expr2) = "("++show expr1++" "++show name++" "++show expr2++")"
    show (NegativeExpr expr) = "-"++show expr
    show (ParenthesesExpr expr) = "("++show expr++")"
    show (ListExpr expr) = show expr
    show (LambdaExpr lambda) = "(\\"++intercalate " | " (map show lambda)++")"
    show (LetExpr scope lets expr) =
        "(let #"++show scope++"# "++show (map snd lets)++" "++show expr++")"
    show (IfExpr c t f) = "(if "++show c++" "++show t++" "++show f++")"
    show (CaseExpr expr pat) = "(case "++show expr++" "++show pat++")"
    show (TypeSignatureExpr expr dataType) = "("++show expr++"::"++show dataType++")"

data Lambda = Lambda Position Int [PatternMatch] Expr

instance Show Lambda where
    show (Lambda _ scope param expr) =
        "#"++show scope++"# "++intercalate " " (map show param)++" -> "++show expr

data CasePattern = CasePattern Int PatternMatch Rhs

instance Show CasePattern where
    show (CasePattern scope pattern expr) = "#"++show scope++"# "++show pattern++" "++show expr

-- Pattern Match

data PatternMatch = PatternMatch Position PrimPatternMatch

instance Show PatternMatch where
    show (PatternMatch _ pattern) = show pattern

data PrimPatternMatch
    = DCPattern ScopedName [PatternMatch] {- data constructor pattern -}
    | LiteralPattern Literal {- literal pattern -}
    | DCOpPattern ScopedName PatternMatch PatternMatch {- infix data constructor pattern -}
    | NegativePattern PatternMatch {- negative pattern -}
    | ListPattern [PatternMatch] {- list pattern -}
    | BindPattern String (Maybe PatternMatch) {- bind pattern, as pattern -}
    | ParenthesesPattern PatternMatch {- Parentheses Pattern -}
    | PatternWithType PatternMatch DataTypeWithContext {- pattern with data type information -}
    | WildCardPattern {- wild card pattern -}

instance Show PrimPatternMatch where
    show (DCPattern name pat) = "("++show name++concatMap ((' ':).show) pat++")"
    show (LiteralPattern literal) = show literal
    show (DCOpPattern name expr1 expr2) = "("++show expr1++" "++show name++" "++show expr2++")"
    show (NegativePattern pat) = "-"++show pat
    show (ListPattern pat) = show pat
    show (BindPattern str pat) = "("++str++maybe "" (("@"++).show) pat++")"
    show (ParenthesesPattern pat) = "("++show pat++")"
    show (PatternWithType pat typename) = "("++show pat++show typename++")"
    show WildCardPattern = "_"

-- Data Type

data DataTypeWithContext = DataTypeWithContext Position [TypeContext] DataType

instance Show DataTypeWithContext where
    show (DataTypeWithContext _ context typename) = show context++show typename

data TypeContext = TypeContext ScopedName String Int

instance Show TypeContext where
    show (TypeContext typeclass typename _) = show typeclass++" "++typename

data DataType = DataType Kind PrimDataType

instance Show DataType where
    show (DataType _ typename) = show typename

data PrimDataType
    = VarType String Int {- variable type -}
    | ConstructorType ScopedName {- constructor type -}
    | ReservedConstructorType String {- reserved constructor type -}
    | ApplyType DataType DataType {- composed type -}
    | ListType DataType {- list type -}
    | FunctionType DataType DataType {- function type -}
    | ParenthesesType DataType {- parentheses type -}

instance Show PrimDataType where
    show (VarType str _) = str
    show (ConstructorType name) = show name
    show (ReservedConstructorType str) = str
    show (ApplyType cons param) = "("++show cons++" "++show param++")"
    show (ListType param) = "["++show param++"]"
    show (FunctionType t1 t2) = "("++show t1++" -> "++show t2++")"
    show (ParenthesesType t) = "("++show t++")"

data Kind
    = AstKind
    | FuncKind Kind Kind
    | VarKind Int String

instance Show Kind where
    show AstKind = "*"
    show (FuncKind a b) = "("++show a++" -> "++show b++")"
    show (VarKind n s) = "#"++show n++"#"++s

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

-- Name

type ModuleName = [String]

type ScopeName = [Int]

data ScopedName = ScopedName ModuleName ScopeName String deriving Eq

instance Show ScopedName where
    show (ScopedName modname scope name) = concatMap (++".") (modname++map show scope)++name

-- Position

type Position = (Int,Int)

-- Output Functions

showWhereClause :: (Show a) => [a] -> String
showWhereClause [] = ""
showWhereClause list = " where\n"++showLayoutList list

showLayoutList :: (Show a) => [a] -> String
showLayoutList = unlines.map ("    "++).concatMap (lines.show)

showContext :: [TypeContext] -> String
showContext [] = ""
showContext context = "("++(intercalate "," (map show context))++") => "

showModuleName :: ModuleName -> String
showModuleName = intercalate "."

