
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
getPos = (\p -> Position (sourceLine p) (sourceColumn p)) <$> getPosition

testPos :: LayoutInfo -> Position -> Parsec s u ()
testPos layout pos | checkLayout layout pos = return ()
                   | otherwise = fail "Layout error"

getPosWithTest :: LayoutInfo -> Parsec s u Position
getPosWithTest layout =
    do pos <- getPos
       testPos layout pos >> return pos

-- Layout

arbitraryLayout :: LayoutInfo
arbitraryLayout = LayoutInfo False 0

checkLayout :: LayoutInfo -> Position -> Bool
checkLayout (LayoutInfo False n) (Position line column) = n <= column
checkLayout (LayoutInfo True n) (Position line column) = n == column

arbitraryElemLayout :: LayoutInfo -> LayoutInfo
arbitraryElemLayout (LayoutInfo t n) = LayoutInfo False n

tailElemLayout :: LayoutInfo -> LayoutInfo
tailElemLayout (LayoutInfo True n) = LayoutInfo False (n+1)
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
    where f (NameToken name@(ScopedName _ str))
              | isUpper (head str) = Just name
              | otherwise = Nothing
          f _ = Nothing

vNameToken :: LayoutInfo -> Parsec TokenStream u ScopedName
vNameToken = getToken f
    where f (NameToken name@(ScopedName _ str))
              | isLower (head str) || '_' == (head str) = Just name
              | otherwise = Nothing
          f _ = Nothing

unscopedNameToken :: LayoutInfo -> Parsec TokenStream u String
unscopedNameToken = getToken f
    where f (NameToken (ScopedName [] str)) = Just str
          f _ = Nothing

unscopedcNameToken :: LayoutInfo -> Parsec TokenStream u String
unscopedcNameToken = getToken f
    where f (NameToken (ScopedName [] str))
              | isUpper (head str) = Just str
              | otherwise = Nothing
          f _ = Nothing

unscopedvNameToken :: LayoutInfo -> Parsec TokenStream u String
unscopedvNameToken = getToken f
    where f (NameToken (ScopedName [] str))
              | isLower (head str) || '_' == (head str) = Just str
              | otherwise = Nothing
          f _ = Nothing

opToken :: LayoutInfo -> Parsec TokenStream u ScopedName
opToken = getToken f
    where f (OpToken name) = Just name
          f _ = Nothing

cOpToken :: LayoutInfo -> Parsec TokenStream u ScopedName
cOpToken = getToken f
    where f (OpToken op@(ScopedName _ str))
              | head str == ':'  = Just op
              | otherwise = Nothing
          f _ = Nothing

vOpToken :: LayoutInfo -> Parsec TokenStream u ScopedName
vOpToken = getToken f
    where f (OpToken op@(ScopedName _ str))
              | head str /= ':'  = Just op
              | otherwise = Nothing
          f _ = Nothing

unscopedOpToken :: LayoutInfo -> Parsec TokenStream u String
unscopedOpToken = getToken f
    where f (OpToken (ScopedName [] str)) = Just str
          f _ = Nothing

unscopedcOpToken :: LayoutInfo -> Parsec TokenStream u String
unscopedcOpToken = getToken f
    where f (OpToken (ScopedName [] str))
              | head str == ':' = Just str
              | otherwise = Nothing
          f _ = Nothing

unscopedvOpToken :: LayoutInfo -> Parsec TokenStream u String
unscopedvOpToken = getToken f
    where f (OpToken (ScopedName [] str))
              | head str /= ':' = Just str
              | otherwise = Nothing
          f _ = Nothing

fixedOpToken :: String -> LayoutInfo -> Parsec TokenStream u ScopedName
fixedOpToken str = getToken f
    where f (OpToken op@(ScopedName _ str'))
              | str == str' = Just op
              | otherwise = Nothing
          f _ = Nothing

reservedToken :: String -> LayoutInfo -> Parsec TokenStream u Token
reservedToken s = getToken f
    where f t@(ReservedToken s') | s == s' = Just t
                                 | otherwise = Nothing
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
unscopedOpParser layout
     = unscopedOpToken layout <|> try (layoutBackquotes layout unscopedNameToken)

unscopedcOpParser :: LayoutInfo -> Parsec TokenStream u String
unscopedcOpParser layout
     = unscopedcOpToken layout <|> try (layoutBackquotes layout unscopedcNameToken)

unscopedvOpParser :: LayoutInfo -> Parsec TokenStream u String
unscopedvOpParser layout
     = unscopedvOpToken layout <|> try (layoutBackquotes layout unscopedvNameToken)

-- Parser Combinators

layoutMany :: LayoutInfo -> (LayoutInfo -> Parsec [s] u a) -> Parsec [s] u [a]
layoutMany layout parser =
    getPosWithTest layout >>= (\(Position _ col) -> many $ parser $ LayoutInfo True col)

layoutMany1 :: LayoutInfo -> (LayoutInfo -> Parsec [s] u a) -> Parsec [s] u [a]
layoutMany1 layout parser =
    getPosWithTest layout >>= (\(Position _ col) -> many1 $ parser $ LayoutInfo True col)

layoutParentheses :: LayoutInfo -> (LayoutInfo -> Parsec TokenStream u a) -> Parsec TokenStream u a
layoutParentheses layout parser = let tlayout = tailElemLayout layout in
    between (reservedToken "(" layout) (reservedToken ")" tlayout) (parser tlayout)

layoutParenthesess :: LayoutInfo -> (LayoutInfo -> Parsec TokenStream u a) -> Parsec TokenStream u a
layoutParenthesess layout parser = let tlayout = tailElemLayout layout in
    between (reservedToken "[" layout) (reservedToken "]" tlayout) (parser tlayout)

layoutBackquotes :: LayoutInfo -> (LayoutInfo -> Parsec TokenStream u a) -> Parsec TokenStream u a
layoutBackquotes layout parser = let tlayout = tailElemLayout layout in
    between (reservedToken "`" layout) (reservedToken "`" tlayout) (parser tlayout)


-- Expression Parser

exprParser :: Int -> LayoutInfo -> Parsec TokenStream u Expr
exprParser 0 = exprWithTypeParser
exprParser 1 = opExprParser
exprParser 2 = choice.flip amap [lambdaExprParser,exprParser 3]
exprParser 3 = choice.flip amap [letParser,ifParser,caseParser,exprParser 4]
exprParser 4 = applyParser
exprParser 5 = choice.flip amap [nameExprParser,literalParser,parenthesesParser,listParser]

guardParser :: String -> LayoutInfo -> Parsec TokenStream u Guard
guardParser str layout = let tlayout = tailElemLayout layout in
    do reservedToken "|" layout
       cond <- exprParser 0 tlayout
       reservedToken str tlayout
       expr <- exprParser 0 tlayout
       return $ Guard cond expr

exprOrGuardParser :: String -> LayoutInfo -> Parsec TokenStream u ExprOrGuard
exprOrGuardParser str layout = let tlayout = tailElemLayout layout in
    (reservedToken str layout >> Left <$> exprParser 0 tlayout) <|>
        (Right <$> layoutMany1 layout (guardParser str))

exprWithTypeParser :: LayoutInfo -> Parsec TokenStream u Expr
exprWithTypeParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       expr <- exprParser 1 layout
       do reservedToken "::" tlayout
          typeName <- typeWithContextParser tlayout
          return $ exprWithType pos expr typeName
          <|> return expr

opExprParser :: LayoutInfo -> Parsec TokenStream u Expr
opExprParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       head <- exprParser 2 layout
       do op <- opParser tlayout
          tail <- opExprParser tlayout
          return $ infixExpr pos op head tail
          <|> return head
    <|> liftM3 (\pos _ body -> negativeExpr pos body)
            getPos (fixedOpToken "-" layout) (opExprParser tlayout)

lambdaExprParser :: LayoutInfo -> Parsec TokenStream u Expr
lambdaExprParser layout =
    do pos <- getPos
       reservedToken "\\" layout
       body <- sepBy1 oneLambda (reservedToken "|" tlayout)
       return $ lambdaExpr pos body
    where
        tlayout = tailElemLayout layout
        oneLambda =
            do pos <- getPos
               params <- many1PatternParser tlayout
               reservedToken "->" tlayout
               expr <- exprParser 0 tlayout
               return $ Lambda pos params expr

nameExprParser :: LayoutInfo -> Parsec TokenStream u Expr
nameExprParser layout = liftM2 nameExpr (getPosWithTest layout) (nameParser layout)

literalParser :: LayoutInfo -> Parsec TokenStream u Expr
literalParser layout = liftM2 literalExpr (getPosWithTest layout) (literalToken layout)

parenthesesParser :: LayoutInfo -> Parsec TokenStream u Expr
parenthesesParser layout =
    liftM2 parenthesesExpr getPos (layoutParenthesess layout (exprParser 0))

listParser :: LayoutInfo -> Parsec TokenStream u Expr
listParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       reservedToken "[" layout
       body <- sepBy (exprParser 0 tlayout) (reservedToken "," tlayout)
       reservedToken "]" tlayout
       return $ listExpr pos body

letParser :: LayoutInfo -> Parsec TokenStream u Expr
letParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       reservedToken "let" layout
       list <- layoutMany1 tlayout let1Parser
       reservedToken "in" tlayout
       expr <- exprParser 0 tlayout
       return $ letExpr pos list expr
    where
        let1Parser layout = let tlayout = tailElemLayout layout in
            liftM2 PrimLet (aPatternParser layout) (exprOrGuardParser "=" tlayout)

ifParser :: LayoutInfo -> Parsec TokenStream u Expr
ifParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       reservedToken "if" layout
       c <- exprParser 0 tlayout
       reservedToken "then" tlayout
       t <- exprParser 0 tlayout
       reservedToken "else" tlayout
       f <- exprParser 0 tlayout
       return $ ifExpr pos c t f

caseParser :: LayoutInfo -> Parsec TokenStream u Expr
caseParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       reservedToken "case" layout
       expr <- exprParser 0 tlayout
       reservedToken "of" tlayout
       list <- layoutMany1 tlayout casePatternParser
       return $ caseExpr pos expr list
    where
        casePatternParser layout = let tlayout = tailElemLayout layout in
            liftM2 CasePattern (aPatternParser layout) (exprOrGuardParser "->" tlayout)

applyParser layout = let tlayout = tailElemLayout layout in
    liftM2 (foldl apply) (exprParser 5 layout) (many (exprParser 5 tlayout))
    where apply l@(Expr pos _) r = applyFunctionExpr pos l r

-- Pattern Match Parser

aPatternParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
aPatternParser = patternParser 0

many1PatternParser :: LayoutInfo -> Parsec TokenStream u [PatternMatch]
many1PatternParser layout = let tlayout = tailElemLayout layout in
    do liftM2 (:) (patternParser 3 layout) (many $ patternParser 3 tlayout)

patternParser :: Int -> LayoutInfo -> Parsec TokenStream u PatternMatch
patternParser 0 = patternWithTypeParser
patternParser 1 = opPatternParser
patternParser 2 = choice.flip amap [dcPatternParser,patternParser 3]
patternParser 3 = choice.flip amap [literalPatternParser,asPatternParser,
    parenthesesPatternParser,listPatternParser,singleDCPatternParser]

patternWithTypeParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
patternWithTypeParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       head <- patternParser 1 layout
       do reservedToken "::" tlayout
          typeName <- typeWithContextParser tlayout
          return $ patternWithType pos head typeName
          <|> return head

opPatternParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
opPatternParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       do head <- patternParser 2 layout
          do cons <- cOpToken tlayout <|> fixedOpToken "+" tlayout
             tail <- opPatternParser tlayout
             return $ dcOpPattern pos cons head tail
             <|> return head
        <|> do fixedOpToken "-" layout
               num <- numLiteralToken tlayout
               return $ literalPattern pos $ case num of
                   (LiteralInt n) -> LiteralInt $ -n
                   (LiteralFloat n) -> LiteralFloat $ -n

dcPatternParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
dcPatternParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
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
       do reservedToken "@" tlayout
          pattern <- patternParser 2 tlayout
          return $ asPattern pos var pattern
          <|> return (bindPattern pos var)

singleDCPatternParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
singleDCPatternParser layout = liftM3 dcPattern getPos (cNameParser layout) (return [])

parenthesesPatternParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
parenthesesPatternParser layout =
    liftM2 parenthesesPattern getPos (layoutParenthesess layout (patternParser 0))

listPatternParser :: LayoutInfo -> Parsec TokenStream u PatternMatch
listPatternParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       reservedToken "[" layout
       body <- sepBy (patternParser 0 tlayout) (reservedToken "," tlayout)
       reservedToken "]" tlayout
       return $ listPattern pos body

-- Type Name Parser

typeWithContextParser :: LayoutInfo -> Parsec TokenStream u DataTypeWithContext
typeWithContextParser layout = let tlayout = tailElemLayout layout in
    liftM3 DataTypeWithContext getPos (classInfoParser layout) (typeParser 0 tlayout)

classInfoParser :: LayoutInfo -> Parsec TokenStream u [TypeContext]
classInfoParser layout = let tlayout = tailElemLayout layout in option [] $ try $
    do reservedToken "(" layout
       body <- sepBy1 (liftM3 TypeContext getPos (cNameParser tlayout) (typeParser 0 tlayout))
           (reservedToken "," tlayout)
       reservedToken ")" tlayout
       reservedToken "=>" tlayout
       return body

typeParser :: Int -> LayoutInfo -> Parsec TokenStream u DataType
typeParser 0 = functionTypeParser
typeParser 1 = choice.flip amap [composedTypeParser,typeParser 2]
typeParser 2 = choice.flip amap
    [listTypeParser,parenthesesTypeParser,constructorTypeParser,variableTypeParser]

functionTypeParser :: LayoutInfo -> Parsec TokenStream u DataType
functionTypeParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       f <- typeParser 1 layout
       do reservedToken "->" tlayout
          p <- typeParser 0 layout
          return $ functionType pos f p
          <|> return f

composedTypeParser :: LayoutInfo -> Parsec TokenStream u DataType
composedTypeParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       head <- typeParser 2 layout
       tail <- many $ typeParser 2 tlayout
       return $ foldl (composedType pos) head tail

listTypeParser :: LayoutInfo -> Parsec TokenStream u DataType
listTypeParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       reservedToken "[" layout
       result <- (listType pos <$> typeParser 0 tlayout)
           <|> return (reservedConstructorType pos "[]")
       reservedToken "]" tlayout
       return result

parenthesesTypeParser :: LayoutInfo -> Parsec TokenStream u DataType
parenthesesTypeParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
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

globalParser :: Parsec TokenStream u Expr
globalParser = exprParser 0 arbitraryLayout <* eof

{-
topDeclParser :: LayoutInfo -> Parsec TokenStream u TopDecl
topDeclParser layout = undefined




fixityDeclParser :: LayoutInfo -> Parsec TokenStream u Decl
fixityDeclParser layout = let tlayout = tailElemLayout layout in
    do pos <- getPos
       fixity <- fixityParser layout
       num <- option Nothing (Just <$> operatorLevelParser tlayout)
    where
        fixityParser :: LayoutInfo -> Parsec TokenStream u Fixity
        fixityParser layout = (reservedToken "infixl" layout >> return Infixl) <|>
            (reservedToken "infix" layout >> return Infix) <|>
            (reservedToken "infixr" layout >> return Infixr)
        operatorLevelParser :: LayoutInfo -> Parsec TokenStream u Int
        operatorLevelParser layout = getToken operatorLevelTest
        operatorLevelTest (LiteralToken (LiteralInt num))
            | (0 <= num && num <= 9) = Just num
            | otherwise = Nothing
        operatorLevelTest _ = Nothing
-}
