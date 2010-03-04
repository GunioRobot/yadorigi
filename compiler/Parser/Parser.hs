
module Yadorigi.Parser.Parser where

import Yadorigi.Common
import Yadorigi.Parser.DataTypes

import Text.Parsec
import Control.Applicative ((<$>),(<*),(*>),(<*>),(<**>))
import Control.Monad
import Data.Char
import Data.Maybe

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
    where f (NameToken name@(ScopedName _ str)) | isUpper (head str) = Just name
          f _ = Nothing

vNameToken :: LayoutInfo -> Parsec TokenStream u ScopedName
vNameToken = getToken f
    where f (NameToken name@(ScopedName _ str)) | isLower (head str) || '_' == (head str) = Just name
          f _ = Nothing

unscopedNameToken :: LayoutInfo -> Parsec TokenStream u String
unscopedNameToken = getToken f
    where f (NameToken (ScopedName [] str)) = Just str
          f _ = Nothing

unscopedcNameToken :: LayoutInfo -> Parsec TokenStream u String
unscopedcNameToken = getToken f
    where f (NameToken (ScopedName [] str)) | isUpper (head str) = Just str
          f _ = Nothing

unscopedvNameToken :: LayoutInfo -> Parsec TokenStream u String
unscopedvNameToken = getToken f
    where f (NameToken (ScopedName [] str)) | isLower (head str) || '_' == (head str) = Just str
          f _ = Nothing

opToken :: LayoutInfo -> Parsec TokenStream u ScopedName
opToken = getToken f
    where f (OpToken name) = Just name
          f _ = Nothing

cOpToken :: LayoutInfo -> Parsec TokenStream u ScopedName
cOpToken = getToken f
    where f (OpToken op@(ScopedName _ str)) | head str == ':' = Just op
          f _ = Nothing

vOpToken :: LayoutInfo -> Parsec TokenStream u ScopedName
vOpToken = getToken f
    where f (OpToken op@(ScopedName _ str)) | head str /= ':' = Just op
          f _ = Nothing

unscopedOpToken :: LayoutInfo -> Parsec TokenStream u String
unscopedOpToken = getToken f
    where f (OpToken (ScopedName [] str)) = Just str
          f _ = Nothing

unscopedcOpToken :: LayoutInfo -> Parsec TokenStream u String
unscopedcOpToken = getToken f
    where f (OpToken (ScopedName [] str)) | head str == ':' = Just str
          f _ = Nothing

unscopedvOpToken :: LayoutInfo -> Parsec TokenStream u String
unscopedvOpToken = getToken f
    where f (OpToken (ScopedName [] str)) | head str /= ':' = Just str
          f _ = Nothing

fixedOpToken :: String -> LayoutInfo -> Parsec TokenStream u ScopedName
fixedOpToken str = getToken f
    where f (OpToken op@(ScopedName _ str')) | str == str' = Just op
          f _ = Nothing

reservedToken :: String -> LayoutInfo -> Parsec TokenStream u Token
reservedToken s = getToken f
    where f t@(ReservedToken s') | s == s' = Just t
          f _ = Nothing

-- Name and Operator Parser

nameParser :: LayoutInfo -> Parsec TokenStream u ScopedName
nameParser layout = nameToken layout <|> try (layoutParentheses layout opToken)

cNameParser :: LayoutInfo -> Parsec TokenStream u ScopedName
cNameParser layout = cNameToken layout <|> try (layoutParentheses layout cOpToken)

vNameParser :: LayoutInfo -> Parsec TokenStream u ScopedName
vNameParser layout = vNameToken layout <|> try (layoutParentheses layout vOpToken)

unscopedNameParser :: LayoutInfo -> Parsec TokenStream u String
unscopedNameParser layout =
    unscopedNameToken layout <|> try (layoutParentheses layout unscopedOpToken)

unscopedcNameParser :: LayoutInfo -> Parsec TokenStream u String
unscopedcNameParser layout =
    unscopedcNameToken layout <|> try (layoutParentheses layout unscopedcOpToken)

unscopedvNameParser :: LayoutInfo -> Parsec TokenStream u String
unscopedvNameParser layout =
    unscopedvNameToken layout <|> try (layoutParentheses layout unscopedvOpToken)

opParser :: LayoutInfo -> Parsec TokenStream u ScopedName
opParser layout = opToken layout <|> try (layoutBackquotes layout nameToken)

cOpParser :: LayoutInfo -> Parsec TokenStream u ScopedName
cOpParser layout = cOpToken layout <|> try (layoutBackquotes layout cNameToken)

vOpParser :: LayoutInfo -> Parsec TokenStream u ScopedName
vOpParser layout = vOpToken layout <|> try (layoutBackquotes layout vNameToken)

unscopedOpParser :: LayoutInfo -> Parsec TokenStream u String
unscopedOpParser layout =
     unscopedOpToken layout <|> try (layoutBackquotes layout unscopedNameToken)

unscopedcOpParser :: LayoutInfo -> Parsec TokenStream u String
unscopedcOpParser layout =
     unscopedcOpToken layout <|> try (layoutBackquotes layout unscopedcNameToken)

unscopedvOpParser :: LayoutInfo -> Parsec TokenStream u String
unscopedvOpParser layout =
     unscopedvOpToken layout <|> try (layoutBackquotes layout unscopedvNameToken)

moduleNameParser :: LayoutInfo -> Parsec TokenStream u ModuleName
moduleNameParser layout =
    do (ScopedName a b) <- cNameToken layout
       return (a++[b])

-- Parser Combinators

offsideRuleMany :: (LayoutInfo -> Parsec [s] u a) -> LayoutInfo -> Parsec [s] u [a]
offsideRuleMany parser layout =
    (getPosWithTest layout >>= \(_,col) -> many $ parser $ Right col) <|> return []

offsideRuleMany1 :: (LayoutInfo -> Parsec [s] u a) -> LayoutInfo -> Parsec [s] u [a]
offsideRuleMany1 parser layout =
    getPosWithTest layout >>= (\(_,col) -> many1 $ parser $ Right col)

layoutParentheses :: LayoutInfo -> (LayoutInfo -> Parsec TokenStream u a) -> Parsec TokenStream u a
layoutParentheses layout parser = let tlayout = tailElemLayout layout in
    between (reservedToken "(" layout) (reservedToken ")" tlayout) (parser tlayout)

layoutBracket :: LayoutInfo -> (LayoutInfo -> Parsec TokenStream u a) -> Parsec TokenStream u a
layoutBracket layout parser = let tlayout = tailElemLayout layout in
    between (reservedToken "[" layout) (reservedToken "]" tlayout) (parser tlayout)

layoutBackquotes :: LayoutInfo -> (LayoutInfo -> Parsec TokenStream u a) -> Parsec TokenStream u a
layoutBackquotes layout parser = let tlayout = tailElemLayout layout in
    between (reservedToken "`" layout) (reservedToken "`" tlayout) (parser tlayout)

layoutChoice :: [LayoutInfo -> Parsec TokenStream u a] -> LayoutInfo -> Parsec TokenStream u a
layoutChoice list layout = choice $ amap layout list

-- Expression Parser

exprParser :: Int -> LayoutInfo -> Parsec TokenStream u Expr
exprParser 0 = exprWithTypeParser
exprParser 1 = opExprParser
exprParser 2 = layoutChoice [lambdaExprParser,exprParser 3]
exprParser 3 = layoutChoice [letParser,ifParser,caseParser,exprParser 4]
exprParser 4 = applyParser
exprParser 5 = layoutChoice [nameExprParser,literalParser,parenthesesParser,listParser]

exprWithTypeParser :: LayoutInfo -> Parsec TokenStream u Expr
exprWithTypeParser layout = do
    let tlayout = tailElemLayout layout
    pos <- getPos
    expr <- exprParser 1 layout
    option expr $
        reservedToken "::" tlayout >> (exprWithType pos expr <$> typeWithContextParser tlayout)

opExprParser :: LayoutInfo -> Parsec TokenStream u Expr
opExprParser layout = do
    let tlayout = tailElemLayout layout
    pos <- getPos
    head <- exprParser 2 layout <|>
        (negativeExpr pos <$> (fixedOpToken "-" layout >> exprParser 2 tlayout))
    option head $ liftM3 (infixExpr pos) (opParser tlayout) (return head) (opExprParser tlayout)

lambdaExprParser :: LayoutInfo -> Parsec TokenStream u Expr
lambdaExprParser layout = do
    pos <- getPos
    reservedToken "\\" layout
    body <- sepBy1 oneLambda (reservedToken "|" tlayout)
    return $ lambdaExpr pos body
    where tlayout = tailElemLayout layout
          oneLambda = do
              pos <- getPos
              params <- many1PatternParser tlayout
              reservedToken "->" tlayout
              expr <- exprParser 0 tlayout
              return $ Lambda pos params expr

nameExprParser :: LayoutInfo -> Parsec TokenStream u Expr
nameExprParser layout = liftM3 nameExpr (getPosWithTest layout) (nameParser layout) (return 0)

literalParser :: LayoutInfo -> Parsec TokenStream u Expr
literalParser layout = liftM2 literalExpr (getPosWithTest layout) (literalToken layout)

parenthesesParser :: LayoutInfo -> Parsec TokenStream u Expr
parenthesesParser layout = liftM2 parenthesesExpr getPos (layoutParentheses layout (exprParser 0))

listParser :: LayoutInfo -> Parsec TokenStream u Expr
listParser layout = liftM2 listExpr getPos
    (layoutBracket layout (\l -> sepBy (exprParser 0 l) (reservedToken "," l)))

letParser :: LayoutInfo -> Parsec TokenStream u Expr
letParser layout = do
    let tlayout = tailElemLayout layout
    pos <- getPos
    reservedToken "let" layout
    list <- offsideRuleMany1 simpleDeclParser tlayout
    reservedToken "in" tlayout
    expr <- exprParser 0 tlayout
    return $ letExpr pos list expr

ifParser :: LayoutInfo -> Parsec TokenStream u Expr
ifParser layout = do
    let tlayout = tailElemLayout layout
    pos <- getPos
    reservedToken "if" layout
    c <- exprParser 0 tlayout
    reservedToken "then" tlayout
    t <- exprParser 0 tlayout
    reservedToken "else" tlayout
    f <- exprParser 0 tlayout
    return $ ifExpr pos c t f

caseParser :: LayoutInfo -> Parsec TokenStream u Expr
caseParser layout = do
    let tlayout = tailElemLayout layout
    pos <- getPos
    reservedToken "case" layout
    expr <- exprParser 0 tlayout
    reservedToken "of" tlayout
    list <- offsideRuleMany1 casePatternParser tlayout
    return $ caseExpr pos expr list
    where casePatternParser layout = let tlayout = tailElemLayout layout in
              liftM2 CasePattern (aPatternParser layout) (rhsParser "->" tlayout)

applyParser layout = let tlayout = tailElemLayout layout in
    liftM2 (foldl apply) (exprParser 5 layout) (many (exprParser 5 tlayout))
    where apply l@(Expr pos _) r = applyFunctionExpr pos l r

-- Pattern Match Parser

aPatternParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
aPatternParser = patternParser 0

many1PatternParser :: LayoutInfo -> Parsec TokenStream u [PatternMatch]
many1PatternParser layout = let tlayout = tailElemLayout layout in
    liftM2 (:) (patternParser 3 layout) (many $ patternParser 3 tlayout)

patternParser :: Int -> LayoutInfo -> Parsec TokenStream u PatternMatch
patternParser 0 = patternWithTypeParser
patternParser 1 = opPatternParser
patternParser 2 = layoutChoice [dcPatternParser,patternParser 3]
patternParser 3 = layoutChoice [literalPatternParser
    ,asPatternParser,parenthesesPatternParser,listPatternParser,singleDCPatternParser]

patternWithTypeParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
patternWithTypeParser layout = do
    let tlayout = tailElemLayout layout
    pos <- getPos
    head <- patternParser 1 layout
    option head $ reservedToken "::" tlayout >>
        (patternWithType pos head <$> typeWithContextParser tlayout)

opPatternParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
opPatternParser layout = do
    let tlayout = tailElemLayout layout
    pos <- getPos
    head <- patternParser 2 layout <|>
        (fixedOpToken "-" layout >> (negativePattern pos <$> patternParser 2 tlayout))
    option head $ liftM3 (dcOpPattern pos)
        (cOpParser tlayout) (return head) (opPatternParser tlayout)

dcPatternParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
dcPatternParser layout = do
    let tlayout = tailElemLayout layout
    pos <- getPos
    cons <- cNameParser layout
    body <- many (patternParser 3 tlayout)
    return $ dcPattern pos cons body

literalPatternParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
literalPatternParser layout =
    liftM2 literalPattern getPos (literalToken layout)

asPatternParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
asPatternParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       var <- unscopedvNameParser layout
       option (bindPattern pos var 0)
           (reservedToken "@" tlayout >> (asPattern pos var 0 <$> patternParser 2 tlayout))

singleDCPatternParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
singleDCPatternParser layout = liftM3 dcPattern getPos (cNameParser layout) (return [])

parenthesesPatternParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
parenthesesPatternParser layout =
    liftM2 parenthesesPattern getPos (layoutParentheses layout (patternParser 0))

listPatternParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
listPatternParser layout = liftM2 listPattern getPos
    (layoutBracket layout (\l -> sepBy (patternParser 0 l) (reservedToken "," l)))

-- Type Name Parser

typeWithContextParser :: LayoutInfo -> Parsec TokenStream u DataTypeWithContext
typeWithContextParser layout = let tlayout = tailElemLayout layout in
    liftM3 DataTypeWithContext getPos (contextParser layout) (typeParser 0 tlayout)

contextParser :: LayoutInfo -> Parsec TokenStream u [TypeContext]
contextParser layout = option [] $ try $ do
    let tlayout = tailElemLayout layout
    reservedToken "(" layout
    body <- sepBy1 (liftM3 TypeContext getPos (cNameParser tlayout) (typeParser 0 tlayout))
        (reservedToken "," tlayout)
    reservedToken ")" tlayout
    reservedToken "=>" tlayout
    return body

typeParser :: Int -> LayoutInfo -> Parsec TokenStream u DataType
typeParser 0 = functionTypeParser
typeParser 1 = layoutChoice [composedTypeParser,typeParser 2]
typeParser 2 = layoutChoice
    [listTypeParser,parenthesesTypeParser,constructorTypeParser,variableTypeParser]

functionTypeParser :: LayoutInfo -> Parsec TokenStream u DataType
functionTypeParser layout = do
    let tlayout = tailElemLayout layout
    pos <- getPos
    f <- typeParser 1 layout
    option f $ reservedToken "->" tlayout >> (functionType pos f <$> typeParser 0 layout)

composedTypeParser :: LayoutInfo -> Parsec TokenStream u DataType
composedTypeParser layout = do
    let tlayout = tailElemLayout layout
    pos <- getPos
    head <- typeParser 2 layout
    tail <- many $ typeParser 2 tlayout
    return $ foldl (composedType pos) head tail

listTypeParser :: LayoutInfo -> Parsec TokenStream u DataType
listTypeParser layout = do
    let tlayout = tailElemLayout layout
    pos <- getPos
    reservedToken "[" layout
    result <- (listType pos <$> typeParser 0 tlayout) <|> return (reservedConstructorType pos "[]")
    reservedToken "]" tlayout
    return result

parenthesesTypeParser :: LayoutInfo -> Parsec TokenStream u DataType
parenthesesTypeParser layout = do
    let tlayout = tailElemLayout layout
    pos <- getPos
    reservedToken "(" layout
    result <- (parenthesesType pos <$> typeParser 0 tlayout) <|>
        (reservedToken "->" tlayout >> return (reservedConstructorType pos "->"))
    reservedToken ")" tlayout
    return result

constructorTypeParser :: LayoutInfo -> Parsec TokenStream u DataType
constructorTypeParser layout = liftM2 constructorType getPos (cNameParser layout)

variableTypeParser :: LayoutInfo -> Parsec TokenStream u DataType
variableTypeParser layout = liftM2 variableType getPos (unscopedvNameParser layout)

-- Top Declaration Parser

topDeclParser :: LayoutInfo -> Parsec TokenStream u TopDecl
topDeclParser = layoutChoice
    [dataDeclParser,typeDeclParser,classDeclParser,instanceDeclParser,declDeclParser]

dataDeclParser :: LayoutInfo -> Parsec TokenStream u TopDecl
dataDeclParser layout = do
    let tlayout = tailElemLayout layout
    pos <- getPos
    reservedToken "data" layout
    context <- contextParser tlayout
    typeName <- unscopedcNameToken tlayout
    params <- many $ unscopedvNameToken tlayout
    reservedToken "=" tlayout
    constructors <- sepBy1
        (liftM2 (,) (unscopedcNameToken tlayout) (many $ typeParser 2 tlayout))
        (reservedToken "|" tlayout)
    return $ dataDecl pos context typeName params constructors

typeDeclParser :: LayoutInfo -> Parsec TokenStream u TopDecl
typeDeclParser layout = do
    let tlayout = tailElemLayout layout
    pos <- getPos
    reservedToken "type" layout
    typeName <- unscopedcNameToken tlayout
    params <- many $ unscopedvNameToken tlayout
    reservedToken "=" tlayout
    body <- typeWithContextParser tlayout
    return $ typeDecl pos typeName params body

classDeclParser :: LayoutInfo -> Parsec TokenStream u TopDecl
classDeclParser layout = do
    let tlayout = tailElemLayout layout
    pos <- getPos
    reservedToken "class" layout
    context <- contextParser tlayout
    className <- unscopedcNameToken tlayout
    param <- unscopedvNameToken tlayout
    body <- option [] $ reservedToken "where" tlayout >> offsideRuleMany declParser tlayout
    return $ classDecl pos context className param body

instanceDeclParser :: LayoutInfo -> Parsec TokenStream u TopDecl
instanceDeclParser layout = do
    let tlayout = tailElemLayout layout
    pos <- getPos
    reservedToken "instance" layout
    context <- contextParser tlayout
    className <- unscopedcNameToken tlayout
    inst <- typeParser 0 tlayout
    body <- option [] $ reservedToken "where" tlayout >> offsideRuleMany declParser tlayout
    return $ instanceDecl pos context className inst body

declDeclParser :: LayoutInfo -> Parsec TokenStream u TopDecl
declDeclParser layout = declToTopDecl <$> declParser layout

-- Declaration Parser

declParser :: LayoutInfo -> Parsec TokenStream u Decl
declParser = layoutChoice [fixityDeclParser,typeSignatureParser,bindDeclParser]

simpleDeclParser :: LayoutInfo -> Parsec TokenStream u Decl
simpleDeclParser = layoutChoice [fixityDeclParser,typeSignatureParser,simpleBindDeclParser]

fixityDeclParser :: LayoutInfo -> Parsec TokenStream u Decl
fixityDeclParser layout = do
    pos <- getPos
    fixity <- fixityParser
    num <- option Nothing (Just <$> operatorLevelParser)
    ops <- sepBy1 (opParser tlayout) (reservedToken "," tlayout)
    return $ fixityDecl pos fixity num ops
    where tlayout = tailElemLayout layout
          fixityParser = (reservedToken "infixl" layout >> return Infixl) <|>
              (reservedToken "infix" layout >> return Infix) <|>
              (reservedToken "infixr" layout >> return Infixr)
          operatorLevelParser = getToken operatorLevelTest tlayout <?> "valid precedence"
          operatorLevelTest (LiteralToken (LiteralInt num)) | (0 <= num && num <= 9) = Just num
          operatorLevelTest _ = Nothing

typeSignatureParser :: LayoutInfo -> Parsec TokenStream u Decl
typeSignatureParser layout = try $ do
    let tlayout = tailElemLayout layout
    pos <- getPos
    name <- unscopedvNameParser layout
    reservedToken "::" tlayout
    typeName <- typeWithContextParser tlayout
    return $ typeSignatureDecl pos name typeName

simpleBindDeclParser :: LayoutInfo -> Parsec TokenStream u Decl
simpleBindDeclParser layout =
    try $ liftM3 bindDecl getPos (bindParser "=" layout) (return [])

bindDeclParser :: LayoutInfo -> Parsec TokenStream u Decl
bindDeclParser layout = try $ let tlayout = tailElemLayout layout in
    liftM3 bindDecl getPos (bindParser "=" layout)
        (option [] $ reservedToken "where" tlayout >> offsideRuleMany declParser tlayout)

-- Left/Right Hand Side Parser, Bind Parser

lhsParser :: LayoutInfo -> Parsec TokenStream u Lhs
lhsParser = layoutChoice [try.functionLhsParser,try.infixLhsParser,try.patternLhsParser]

functionLhsParser :: LayoutInfo -> Parsec TokenStream u Lhs
functionLhsParser layout = let tlayout = tailElemLayout layout in
    liftM2 FunctionLhs (unscopedvNameParser layout) (many1 (patternParser 3 tlayout))

infixLhsParser :: LayoutInfo -> Parsec TokenStream u Lhs
infixLhsParser layout = let tlayout = tailElemLayout layout in
    liftM3 (flip InfixLhs)
        (patternParser 1 layout) (unscopedvOpParser tlayout) (patternParser 1 tlayout)

patternLhsParser :: LayoutInfo -> Parsec TokenStream u Lhs
patternLhsParser layout = PatternLhs "" <$> patternParser 0 layout

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

bindParser :: String -> LayoutInfo -> Parsec TokenStream u Bind
bindParser str layout = do
    let tlayout = tailElemLayout layout
    lhs <- lhsParser layout
    rhs <- rhsParser str tlayout
    return $ Bind lhs rhs

-- Module Parser

moduleParser :: Parsec TokenStream u Module
moduleParser = do
    modname <- option [] $ reservedToken "module" layout
        >> moduleNameParser tlayout <* reservedToken "where" tlayout
    importDecls <- offsideRuleMany importDeclParser layout
    topDecls <- offsideRuleMany topDeclParser layout
    eof
    return $ Module modname importDecls topDecls
    where layout = Right 1
          tlayout = tailElemLayout layout
          importDeclParser layout =
              reservedToken "import" layout >> moduleNameParser (tailElemLayout layout)

