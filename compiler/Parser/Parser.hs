
module Yadorigi.Parser.Parser where

import Yadorigi.Common
import Yadorigi.Syntax
import Yadorigi.Parser.DataTypes

import Text.Parsec
import Control.Applicative ((<$>),(<$),(<*),(*>),(<*>),(<**>))
import Control.Monad
import Data.Char
import Data.Maybe

-- Data Types

type LayoutInfo = Either Int Int

-- Position

getPos :: Parsec s u Position
getPos = (\p -> (sourceLine p,sourceColumn p)) <$> getPosition

testPos :: LayoutInfo -> Position -> Parsec s u ()
testPos layout pos | checkLayout layout pos = return ()
                   | otherwise = fail "Layout error"

getPosWithTest :: LayoutInfo -> Parsec s u Position
getPosWithTest layout =
    do pos <- getPos
       testPos layout pos >> return pos

-- Layout

arbitraryLayout :: LayoutInfo
arbitraryLayout = Left 1

checkLayout :: LayoutInfo -> Position -> Bool
checkLayout (Left n) (line,column) = n <= column
checkLayout (Right n) (line,column) = n == column

arbitraryElemLayout :: LayoutInfo -> LayoutInfo
arbitraryElemLayout (Right n) = Left n
arbitraryElemLayout layout = layout

tailElemLayout :: LayoutInfo -> LayoutInfo
tailElemLayout (Right n) = Left (n+1)
tailElemLayout layout = layout

-- Tokenizer

getToken :: (Token -> Maybe a) -> LayoutInfo -> Parsec TokenStream u a
getToken f layout =
    do getPosWithTest layout
       token show (\(Token' pos _) -> pos) (\(Token' _ token) -> f token)

literalToken :: LayoutInfo -> Parsec TokenStream u Literal
literalToken = getToken f
    where f (LiteralToken literal) = Just literal
          f _ = Nothing

numLiteralToken :: LayoutInfo -> Parsec TokenStream u Literal
numLiteralToken = getToken f
    where f (LiteralToken (LiteralInt n)) = Just (LiteralInt n)
          f (LiteralToken (LiteralFloat n)) = Just (LiteralFloat n)
          f _ = Nothing

nameToken :: LayoutInfo -> Parsec TokenStream u ScopedName
nameToken = getToken f
    where f (NameToken name) = Just name
          f _ = Nothing

cNameToken :: LayoutInfo -> Parsec TokenStream u ScopedName
cNameToken = getToken f
    where f (NameToken name@(ScopedName _ _ str)) | isUpper (head str) = Just name
          f _ = Nothing

vNameToken :: LayoutInfo -> Parsec TokenStream u ScopedName
vNameToken = getToken f
    where f (NameToken name@(ScopedName _ _ str))
              | isLower (head str) || '_' == (head str) = Just name
          f _ = Nothing

unscopedNameToken :: LayoutInfo -> Parsec TokenStream u String
unscopedNameToken = getToken f
    where f (NameToken (ScopedName [] _ str)) = Just str
          f _ = Nothing

unscopedcNameToken :: LayoutInfo -> Parsec TokenStream u String
unscopedcNameToken = getToken f
    where f (NameToken (ScopedName [] _ str)) | isUpper (head str) = Just str
          f _ = Nothing

unscopedvNameToken :: LayoutInfo -> Parsec TokenStream u String
unscopedvNameToken = getToken f
    where f (NameToken (ScopedName [] _ str)) | isLower (head str) || '_' == (head str) = Just str
          f _ = Nothing

fixedNameToken :: String -> LayoutInfo -> Parsec TokenStream u ScopedName
fixedNameToken str = getToken f
    where f (NameToken op@(ScopedName [] _ str')) | str == str' = Just op
          f _ = Nothing

opToken :: LayoutInfo -> Parsec TokenStream u ScopedName
opToken = getToken f
    where f (OpToken name) = Just name
          f _ = Nothing

cOpToken :: LayoutInfo -> Parsec TokenStream u ScopedName
cOpToken = getToken f
    where f (OpToken op@(ScopedName _ _ str)) | head str == ':' = Just op
          f _ = Nothing

vOpToken :: LayoutInfo -> Parsec TokenStream u ScopedName
vOpToken = getToken f
    where f (OpToken op@(ScopedName _ _ str)) | head str /= ':' = Just op
          f _ = Nothing

unscopedOpToken :: LayoutInfo -> Parsec TokenStream u String
unscopedOpToken = getToken f
    where f (OpToken (ScopedName [] _ str)) = Just str
          f _ = Nothing

unscopedcOpToken :: LayoutInfo -> Parsec TokenStream u String
unscopedcOpToken = getToken f
    where f (OpToken (ScopedName [] _ str)) | head str == ':' = Just str
          f _ = Nothing

unscopedvOpToken :: LayoutInfo -> Parsec TokenStream u String
unscopedvOpToken = getToken f
    where f (OpToken (ScopedName [] _ str)) | head str /= ':' = Just str
          f _ = Nothing

fixedOpToken :: String -> LayoutInfo -> Parsec TokenStream u ScopedName
fixedOpToken str = getToken f
    where f (OpToken op@(ScopedName [] _ str')) | str == str' = Just op
          f _ = Nothing

reservedToken :: String -> LayoutInfo -> Parsec TokenStream u Token
reservedToken s = getToken f
    where f t@(ReservedToken s') | s == s' = Just t
          f _ = Nothing

-- Name and Operator Parser

nameParser :: LayoutInfo -> Parsec TokenStream u ScopedName
nameParser layout = nameToken layout <|> try (layoutParentheses opToken layout)

cNameParser :: LayoutInfo -> Parsec TokenStream u ScopedName
cNameParser layout = cNameToken layout <|> try (layoutParentheses cOpToken layout)

vNameParser :: LayoutInfo -> Parsec TokenStream u ScopedName
vNameParser layout = vNameToken layout <|> try (layoutParentheses vOpToken layout)

unscopedNameParser :: LayoutInfo -> Parsec TokenStream u String
unscopedNameParser layout =
    unscopedNameToken layout <|> try (layoutParentheses unscopedOpToken layout)

unscopedcNameParser :: LayoutInfo -> Parsec TokenStream u String
unscopedcNameParser layout =
    unscopedcNameToken layout <|> try (layoutParentheses unscopedcOpToken layout)

unscopedvNameParser :: LayoutInfo -> Parsec TokenStream u String
unscopedvNameParser layout =
    unscopedvNameToken layout <|> try (layoutParentheses unscopedvOpToken layout)

opParser :: LayoutInfo -> Parsec TokenStream u ScopedName
opParser layout = opToken layout <|> try (layoutBackquotes nameToken layout)

cOpParser :: LayoutInfo -> Parsec TokenStream u ScopedName
cOpParser layout = cOpToken layout <|> try (layoutBackquotes cNameToken layout)

vOpParser :: LayoutInfo -> Parsec TokenStream u ScopedName
vOpParser layout = vOpToken layout <|> try (layoutBackquotes vNameToken layout)

unscopedOpParser :: LayoutInfo -> Parsec TokenStream u String
unscopedOpParser layout =
     unscopedOpToken layout <|> try (layoutBackquotes unscopedNameToken layout)

unscopedcOpParser :: LayoutInfo -> Parsec TokenStream u String
unscopedcOpParser layout =
     unscopedcOpToken layout <|> try (layoutBackquotes unscopedcNameToken layout)

unscopedvOpParser :: LayoutInfo -> Parsec TokenStream u String
unscopedvOpParser layout =
     unscopedvOpToken layout <|> try (layoutBackquotes unscopedvNameToken layout)

moduleNameParser :: LayoutInfo -> Parsec TokenStream u ModuleName
moduleNameParser layout =
    do (ScopedName a _ b) <- cNameToken layout
       return (a++[b])

-- Parser Combinators

offsideRuleMany :: (LayoutInfo -> Parsec [s] u a) -> LayoutInfo -> Parsec [s] u [a]
offsideRuleMany parser layout = (getPosWithTest layout >>= many.parser.Right .snd) <|> return []

offsideRuleMany1 :: (LayoutInfo -> Parsec [s] u a) -> LayoutInfo -> Parsec [s] u [a]
offsideRuleMany1 parser layout = getPosWithTest layout >>= many1.parser.Right .snd

layoutParentheses :: (LayoutInfo -> Parsec TokenStream u a) -> LayoutInfo -> Parsec TokenStream u a
layoutParentheses parser layout = let tlayout = tailElemLayout layout in
    between (reservedToken "(" layout) (reservedToken ")" tlayout) (parser tlayout)

layoutBracket :: (LayoutInfo -> Parsec TokenStream u a) -> LayoutInfo -> Parsec TokenStream u a
layoutBracket parser layout = let tlayout = tailElemLayout layout in
    between (reservedToken "[" layout) (reservedToken "]" tlayout) (parser tlayout)

layoutBackquotes :: (LayoutInfo -> Parsec TokenStream u a) -> LayoutInfo -> Parsec TokenStream u a
layoutBackquotes parser layout = let tlayout = tailElemLayout layout in
    between (reservedToken "`" layout) (reservedToken "`" tlayout) (parser tlayout)

layoutChoice :: [LayoutInfo -> Parsec TokenStream u a] -> LayoutInfo -> Parsec TokenStream u a
layoutChoice list layout = choice $ amap layout list

-- Module Parser

moduleParser :: Parsec TokenStream u Module
moduleParser = do
    let layout = Right 1
    let tlayout = tailElemLayout layout
    (modname,exportList) <- option ([],Nothing) $ do
        reservedToken "module" layout
        modname <- option [] $ moduleNameParser tlayout
        exportList <- option Nothing $ Just <$>
            (layoutParentheses (\l -> sepBy (exportEntityParser l) (reservedToken "," l)) tlayout)
        reservedToken "where" tlayout
        return (modname,exportList)
    imports <- offsideRuleMany importParser layout
    topDecls <- offsideRuleMany topDeclParser layout
    eof
    return $ Module modname exportList imports topDecls
    where nameExportEntityParser :: LayoutInfo -> Parsec TokenStream u ExportEntity
          nameExportEntityParser layout = do
              let tlayout = tailElemLayout layout
              entity <- nameParser layout
              children <- (try $ layoutParentheses
                  (\l -> Just <$> sepBy (unscopedNameParser l) (reservedToken "," l)) tlayout) <|>
                  (try $ layoutParentheses (\l -> Nothing <$ fixedOpToken ".." l) tlayout) <|>
                  (return $ Just [])
              return $ NameExportEntity entity children
          moduleExportEntityParser :: LayoutInfo -> Parsec TokenStream u ExportEntity
          moduleExportEntityParser layout = do
              let tlayout = tailElemLayout layout
              reservedToken "module" layout
              ModuleExportEntity <$> moduleNameParser tlayout
          exportEntityParser :: LayoutInfo -> Parsec TokenStream u ExportEntity
          exportEntityParser = layoutChoice [nameExportEntityParser,moduleExportEntityParser]

importParser :: LayoutInfo -> Parsec TokenStream u Import
importParser layout = do
    let tlayout = tailElemLayout layout
    pos <- getPos
    reservedToken "import" layout
    qualified <- option False $ fixedNameToken "qualified" tlayout >> return True
    modname <- moduleNameParser tlayout
    alias <- option Nothing $ Just <$> (fixedNameToken "as" tlayout >> moduleNameParser tlayout)
    importList <- option Nothing $ do
        hiding <- option False $ fixedNameToken "hiding" tlayout >> return True
        importList <-
            layoutParentheses (\l -> sepBy (importEntityParser l) (reservedToken "," l)) tlayout
        return $ Just (hiding,importList)
    return $ Import pos qualified modname alias importList
    where importEntityParser :: LayoutInfo -> Parsec TokenStream u ImportEntity
          importEntityParser layout = do
              let tlayout = tailElemLayout layout
              entity <- unscopedNameParser layout
              children <- (try $ layoutParentheses
                  (\l -> Just <$> sepBy (unscopedNameParser l) (reservedToken "," l)) tlayout) <|>
                  (try $ layoutParentheses (\l -> Nothing <$ fixedOpToken ".." l) tlayout) <|>
                  (return $ Just [])
              return $ ImportEntity entity children

-- Declaration Parser

topDeclParser :: LayoutInfo -> Parsec TokenStream u Decl
topDeclParser layout = liftM3 Decl getPos (return 0) (layoutChoice [dataDeclParser,typeDeclParser,
    classDeclParser,instanceDeclParser,fixityDeclParser,typeSignatureParser,bindDeclParser] layout)

declParser :: LayoutInfo -> Parsec TokenStream u Decl
declParser layout = liftM3 Decl getPos (return 0)
    (layoutChoice [fixityDeclParser,typeSignatureParser,bindDeclParser] layout)

dataDeclParser :: LayoutInfo -> Parsec TokenStream u PrimDecl
dataDeclParser layout = do
    let tlayout = tailElemLayout layout
    reservedToken "data" layout
    context <- contextParser tlayout
    typeName <- unscopedcNameToken tlayout
    params <- many $ unscopedvNameToken tlayout
    reservedToken "=" tlayout
    constructors <- sepBy1
        (liftM2 (,) (unscopedcNameToken tlayout) (many $ typeParser 2 tlayout))
        (reservedToken "|" tlayout)
    return $ DataPrimDecl context typeName params constructors

typeDeclParser :: LayoutInfo -> Parsec TokenStream u PrimDecl
typeDeclParser layout = do
    let tlayout = tailElemLayout layout
    reservedToken "type" layout
    typeName <- unscopedcNameToken tlayout
    params <- many $ unscopedvNameToken tlayout
    reservedToken "=" tlayout
    body <- typeWithContextParser tlayout
    return $ TypePrimDecl typeName params body

classDeclParser :: LayoutInfo -> Parsec TokenStream u PrimDecl
classDeclParser layout = do
    let tlayout = tailElemLayout layout
    reservedToken "class" layout
    context <- contextParser tlayout
    className <- unscopedcNameToken tlayout
    param <- unscopedvNameToken tlayout
    body <- option [] $ reservedToken "where" tlayout >> offsideRuleMany declParser tlayout
    return $ ClassPrimDecl context className param body

instanceDeclParser :: LayoutInfo -> Parsec TokenStream u PrimDecl
instanceDeclParser layout = do
    let tlayout = tailElemLayout layout
    reservedToken "instance" layout
    context <- contextParser tlayout
    className <- cNameToken tlayout
    inst <- typeParser 0 tlayout
    body <- option [] $ reservedToken "where" tlayout >> offsideRuleMany declParser tlayout
    return $ InstancePrimDecl context className inst body

fixityDeclParser :: LayoutInfo -> Parsec TokenStream u PrimDecl
fixityDeclParser layout = do
    fixity <- fixityParser
    num <- option Nothing (Just <$> operatorLevelParser)
    ops <- sepBy1 (unscopedOpParser tlayout) (reservedToken "," tlayout)
    return $ FixityPrimDecl fixity num ops
    where tlayout = tailElemLayout layout
          fixityParser = (reservedToken "infixl" layout >> return Infixl) <|>
              (reservedToken "infix" layout >> return Infix) <|>
              (reservedToken "infixr" layout >> return Infixr)
          operatorLevelParser = getToken operatorLevelTest tlayout <?> "valid precedence"
          operatorLevelTest (LiteralToken (LiteralInt num)) | (0 <= num && num <= 9) = Just num
          operatorLevelTest _ = Nothing

typeSignatureParser :: LayoutInfo -> Parsec TokenStream u PrimDecl
typeSignatureParser layout = try $ do
    let tlayout = tailElemLayout layout
    name <- unscopedvNameParser layout
    reservedToken "::" tlayout
    typeName <- typeWithContextParser tlayout
    return $ TypeSignaturePrimDecl name typeName

bindDeclParser :: LayoutInfo -> Parsec TokenStream u PrimDecl
bindDeclParser layout = try $ let tlayout = tailElemLayout layout in
    liftM2 BindPrimDecl (bindParser "=" layout)
        (option [] $ reservedToken "where" tlayout >> offsideRuleMany declParser tlayout)

simpleBindDeclParser :: LayoutInfo -> Parsec TokenStream u PrimDecl
simpleBindDeclParser layout =
    try $ liftM2 BindPrimDecl (bindParser "=" layout) (return [])

-- Left/Right Hand Side Parser, Bind Parser

bindParser :: String -> LayoutInfo -> Parsec TokenStream u Bind
bindParser str layout = do
    let tlayout = tailElemLayout layout
    lhs <- lhsParser layout
    rhs <- rhsParser str tlayout
    return $ Bind lhs rhs

lhsParser :: LayoutInfo -> Parsec TokenStream u Lhs
lhsParser = layoutChoice [try.functionLhsParser,try.infixLhsParser,try.patternLhsParser]

functionLhsParser :: LayoutInfo -> Parsec TokenStream u Lhs
functionLhsParser layout = let tlayout = tailElemLayout layout in
    liftM2 FunctionLhs (unscopedvNameParser layout) (many1PatternParser tlayout)

infixLhsParser :: LayoutInfo -> Parsec TokenStream u Lhs
infixLhsParser layout = let tlayout = tailElemLayout layout in
    liftM3 (flip InfixLhs)
        (patternParser 1 layout) (unscopedvOpParser tlayout) (patternParser 1 tlayout)

patternLhsParser :: LayoutInfo -> Parsec TokenStream u Lhs
patternLhsParser layout = PatternLhs <$> patternParser 0 layout

rhsParser :: String -> LayoutInfo -> Parsec TokenStream u Rhs
rhsParser str layout = let tlayout = tailElemLayout layout in
    (reservedToken str layout >> (ExprRhs <$> exprParser 0 tlayout)) <|>
        (GuardRhs <$> offsideRuleMany1 (guardParser str) layout)
    where guardParser str layout = do
              let tlayout = tailElemLayout layout
              reservedToken "|" layout
              cond <- exprParser 0 tlayout
              reservedToken str tlayout
              expr <- exprParser 0 tlayout
              return $ Guard cond expr

-- Expression Parser

exprParser :: Int -> LayoutInfo -> Parsec TokenStream u Expr
exprParser n layout = liftM2 Expr getPos (primExprParser n layout)

primExprParser :: Int -> LayoutInfo -> Parsec TokenStream u PrimExpr
primExprParser 0 = exprWithTypeParser
primExprParser 1 = opExprParser
primExprParser 2 = layoutChoice [negativeExprParser,primExprParser 3]
primExprParser 3 = layoutChoice [lambdaExprParser,primExprParser 4]
primExprParser 4 = layoutChoice [letParser,ifParser,caseParser,primExprParser 5]
primExprParser 5 = applyParser
primExprParser 6 = layoutChoice [nameExprParser,literalParser,parenthesesParser,listParser]

primExpr :: Expr -> PrimExpr
primExpr (Expr _ expr) = expr

exprWithTypeParser :: LayoutInfo -> Parsec TokenStream u PrimExpr
exprWithTypeParser layout = do
    let tlayout = tailElemLayout layout
    expr <- exprParser 1 layout
    option (primExpr expr) $
        reservedToken "::" tlayout >> (TypeSignaturePrimExpr expr <$> typeWithContextParser tlayout)

opExprParser :: LayoutInfo -> Parsec TokenStream u PrimExpr
opExprParser layout = do
    let tlayout = tailElemLayout layout
    head <- exprParser 2 layout
    option (primExpr head) $
        liftM3 InfixPrimExpr (opParser tlayout) (return head) (exprParser 1 tlayout)

negativeExprParser :: LayoutInfo -> Parsec TokenStream u PrimExpr
negativeExprParser layout = let tlayout = tailElemLayout layout in
    NegativePrimExpr <$> (fixedOpToken "-" layout >> exprParser 2 tlayout)

lambdaExprParser :: LayoutInfo -> Parsec TokenStream u PrimExpr
lambdaExprParser layout = do
    reservedToken "\\" layout
    body <- sepBy1 oneLambda (reservedToken "|" tlayout)
    return $ LambdaPrimExpr body
    where tlayout = tailElemLayout layout
          oneLambda = do
              pos <- getPos
              params <- many1PatternParser tlayout
              reservedToken "->" tlayout
              expr <- exprParser 0 tlayout
              return $ Lambda pos 0 params expr

letParser :: LayoutInfo -> Parsec TokenStream u PrimExpr
letParser layout = do
    let tlayout = tailElemLayout layout
    reservedToken "let" layout
    list <- offsideRuleMany1 letDeclParser tlayout
    reservedToken "in" tlayout
    expr <- exprParser 0 tlayout
    return $ LetPrimExpr 0 list expr

letDeclParser :: LayoutInfo -> Parsec TokenStream u (Position,PrimDecl)
letDeclParser layout = liftM2 (,) getPos
    (layoutChoice [fixityDeclParser,typeSignatureParser,simpleBindDeclParser] layout)

ifParser :: LayoutInfo -> Parsec TokenStream u PrimExpr
ifParser layout = do
    let tlayout = tailElemLayout layout
    reservedToken "if" layout
    c <- exprParser 0 tlayout
    reservedToken "then" tlayout
    t <- exprParser 0 tlayout
    reservedToken "else" tlayout
    f <- exprParser 0 tlayout
    return $ IfPrimExpr c t f

caseParser :: LayoutInfo -> Parsec TokenStream u PrimExpr
caseParser layout = do
    reservedToken "case" layout
    expr <- exprParser 0 tlayout
    reservedToken "of" tlayout
    list <- offsideRuleMany1 casePatternParser tlayout
    return $ CasePrimExpr expr list
    where tlayout = tailElemLayout layout
          casePatternParser layout = let tlayout = tailElemLayout layout in
              liftM2 (CasePattern 0) (aPatternParser layout) (rhsParser "->" tlayout)

applyParser :: LayoutInfo -> Parsec TokenStream u PrimExpr
applyParser layout = do
    let tlayout = tailElemLayout layout
    head <- exprParser 6 layout
    option (primExpr head) $ ApplyFunctionPrimExpr head <$> exprParser 5 tlayout

nameExprParser :: LayoutInfo -> Parsec TokenStream u PrimExpr
nameExprParser layout = NamePrimExpr <$> nameParser layout

literalParser :: LayoutInfo -> Parsec TokenStream u PrimExpr
literalParser layout = LiteralPrimExpr <$> literalToken layout

parenthesesParser :: LayoutInfo -> Parsec TokenStream u PrimExpr
parenthesesParser layout = ParenthesesPrimExpr <$> layoutParentheses (exprParser 0) layout

listParser :: LayoutInfo -> Parsec TokenStream u PrimExpr
listParser layout = ListPrimExpr <$>
    layoutBracket (\l -> sepBy (exprParser 0 l) (reservedToken "," l)) layout

-- Pattern Match Parser

aPatternParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
aPatternParser = patternParser 0

many1PatternParser :: LayoutInfo -> Parsec TokenStream u [PatternMatch]
many1PatternParser layout = let tlayout = tailElemLayout layout in
    liftM2 (:) (patternParser 4 layout) (many $ patternParser 4 tlayout)

patternParser :: Int -> LayoutInfo -> Parsec TokenStream u PatternMatch
patternParser n layout = liftM2 PatternMatch getPos (primPatternParser n layout)

primPatternParser :: Int -> LayoutInfo -> Parsec TokenStream u PrimPatternMatch
primPatternParser 0 = patternWithTypeParser
primPatternParser 1 = opPatternParser
primPatternParser 2 = layoutChoice [negativePatternParser,primPatternParser 3]
primPatternParser 3 = layoutChoice [dcPatternParser,primPatternParser 4]
primPatternParser 4 = layoutChoice [literalPatternParser
    ,asPatternParser,parenthesesPatternParser,listPatternParser,singleDCPatternParser]

primPattern :: PatternMatch -> PrimPatternMatch
primPattern (PatternMatch _ pattern) = pattern

patternWithTypeParser :: LayoutInfo -> Parsec TokenStream u PrimPatternMatch
patternWithTypeParser layout = do
    let tlayout = tailElemLayout layout
    head <- patternParser 1 layout
    option (primPattern head) $ PrimPatternWithType head <$>
        (reservedToken "::" tlayout >> typeWithContextParser tlayout)

opPatternParser :: LayoutInfo -> Parsec TokenStream u PrimPatternMatch
opPatternParser layout = do
    let tlayout = tailElemLayout layout
    head <- patternParser 2 layout
    option (primPattern head) $
        liftM3 DCOpPrimPattern (cOpParser tlayout) (return head) (patternParser 1 tlayout)

negativePatternParser :: LayoutInfo -> Parsec TokenStream u PrimPatternMatch
negativePatternParser layout = let tlayout = tailElemLayout layout in
    NegativePrimPattern <$> (fixedOpToken "-" layout >> patternParser 2 tlayout)

dcPatternParser :: LayoutInfo -> Parsec TokenStream u PrimPatternMatch
dcPatternParser layout = liftM2 DCPrimPattern
    (cNameParser layout) (many (patternParser 4 (tailElemLayout layout)))

literalPatternParser :: LayoutInfo -> Parsec TokenStream u PrimPatternMatch
literalPatternParser layout = LiteralPrimPattern <$> literalToken layout

asPatternParser :: LayoutInfo -> Parsec TokenStream u PrimPatternMatch
asPatternParser layout = do
    let tlayout = tailElemLayout layout
    var <- unscopedvNameParser layout
    if var == "_"
        then return PrimWildCardPattern
        else option (BindPrimPattern var Nothing) $
            BindPrimPattern var <$> Just <$> (reservedToken "@" tlayout >> patternParser 4 tlayout)

singleDCPatternParser :: LayoutInfo -> Parsec TokenStream u PrimPatternMatch
singleDCPatternParser layout = flip DCPrimPattern [] <$> cNameParser layout

parenthesesPatternParser :: LayoutInfo -> Parsec TokenStream u PrimPatternMatch
parenthesesPatternParser layout =
    ParenthesesPrimPattern <$> layoutParentheses (patternParser 0) layout

listPatternParser :: LayoutInfo -> Parsec TokenStream u PrimPatternMatch
listPatternParser layout = ListPrimPattern <$>
    layoutBracket (\l -> sepBy (patternParser 0 l) (reservedToken "," l)) layout

-- Type Name Parser

typeWithContextParser :: LayoutInfo -> Parsec TokenStream u DataTypeWithContext
typeWithContextParser layout = let tlayout = tailElemLayout layout in
    liftM3 DataTypeWithContext getPos (contextParser layout) (typeParser 0 tlayout)

contextParser :: LayoutInfo -> Parsec TokenStream u [TypeContext]
contextParser layout = let tlayout = tailElemLayout layout in
    option [] $ try $
        (layoutParentheses (\l -> sepBy1 (oneContext l) (reservedToken "," l)) layout <|>
        (return <$> oneContext layout)) <* reservedToken "=>" tlayout
    where oneContext layout = let tlayout = tailElemLayout layout in
              liftM3 TypeContext getPos (cNameParser layout) (typeParser 0 tlayout)

typeParser :: Int -> LayoutInfo -> Parsec TokenStream u DataType
typeParser n layout = liftM2 DataType getPos (primTypeParser n layout)

primTypeParser :: Int -> LayoutInfo -> Parsec TokenStream u PrimDataType
primTypeParser 0 = functionTypeParser
primTypeParser 1 = layoutChoice [composedTypeParser,primTypeParser 2]
primTypeParser 2 = layoutChoice
    [listTypeParser,parenthesesTypeParser,constructorTypeParser,variableTypeParser]

primDataType :: DataType -> PrimDataType
primDataType (DataType _ dataType) = dataType

functionTypeParser :: LayoutInfo -> Parsec TokenStream u PrimDataType
functionTypeParser layout = do
    let tlayout = tailElemLayout layout
    f <- typeParser 1 layout
    option (primDataType f) $ FunctionPrimType f <$>
        (reservedToken "->" tlayout >> typeParser 0 layout)

composedTypeParser :: LayoutInfo -> Parsec TokenStream u PrimDataType
composedTypeParser layout = do
    let tlayout = tailElemLayout layout
    pos <- getPos
    head <- typeParser 2 layout
    tail <- many $ typeParser 2 tlayout
    return $ primDataType $ foldl (((.).(.)) (DataType pos) ComposedPrimType) head tail

listTypeParser :: LayoutInfo -> Parsec TokenStream u PrimDataType
listTypeParser layout = do
    let tlayout = tailElemLayout layout
    reservedToken "[" layout
    result <- option (ReservedConstructorPrimType "[]") (ListPrimType <$> typeParser 0 tlayout)
    reservedToken "]" tlayout
    return result

parenthesesTypeParser :: LayoutInfo -> Parsec TokenStream u PrimDataType
parenthesesTypeParser layout = do
    let tlayout = tailElemLayout layout
    reservedToken "(" layout
    result <- (ParenthesesPrimType <$> typeParser 0 tlayout) <|>
        (reservedToken "->" tlayout >> return (ReservedConstructorPrimType "->"))
    reservedToken ")" tlayout
    return result

constructorTypeParser :: LayoutInfo -> Parsec TokenStream u PrimDataType
constructorTypeParser layout = ConstructorPrimType <$> (cNameParser layout)

variableTypeParser :: LayoutInfo -> Parsec TokenStream u PrimDataType
variableTypeParser layout = VariablePrimType <$> (unscopedvNameParser layout)

