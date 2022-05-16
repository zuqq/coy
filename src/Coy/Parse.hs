{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Coy.Parse where

import Control.Applicative (many, (<|>))
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixN, Postfix, Prefix), makeExprParser)
import Data.Bifunctor (first)
import Data.Char (isAlphaNum, isAscii, isAsciiLower, isAsciiUpper, isDigit, isPrint)
import Data.Functor (void, ($>))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Text.Megaparsec.Error (errorBundlePretty)

import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Text.Megaparsec as Parser
import qualified Text.Megaparsec.Char as Parser
import qualified Text.Megaparsec.Char.Lexer

import Coy.Syntax

data ModuleItem
    = TypeDefItem (Located (TypeDef 'Unchecked))
    | ConstDefItem (Located (ConstDef 'Unchecked))
    | FnDefItem (Located (FnDef 'Unchecked))
    deriving Show

parse :: FilePath -> Text -> Either String (Module 'Unchecked)
parse filePath input = do
    items <- first errorBundlePretty (Parser.parse parseModule filePath input)

    let typeDefs = [d | TypeDefItem d <- items]

    let constDefs = [d | ConstDefItem d <- items]

    let fnDefs = [d | FnDefItem d <- items]

    pure (UncheckedModule typeDefs constDefs fnDefs)
  where
    parseModule = many parseModuleItem

    parseModuleItem =
        fmap TypeDefItem (located typeDef)
        <|> fmap ConstDefItem (located constDef)
        <|> fmap FnDefItem (located fnDef)

type Parser = Parsec Void Text

located :: Parser a -> Parser (Located a)
located p = do
    location <- fmap Location Parser.getOffset
    result <- p
    pure (Located location result)

typeDef :: Parser (TypeDef 'Unchecked)
typeDef = structDef <|> enumDef
  where
    structDef = do
        "struct" *> space1
        n <- structName
        ts <- parenthesized (commaSeparated typeName)
        semicolon
        pure (StructDef n (Vector.fromList ts))

    enumDef = do
        "enum" *> space1
        n <- enumName
        vs <- withinBraces (commaSeparated enumVariant)
        pure (EnumDef n vs)

enumVariant :: Parser (EnumVariant 'Unchecked)
enumVariant = do
    v <- enumVariantName
    ts <- parenthesized (commaSeparated typeName)
    pure (EnumVariant v (Vector.fromList ts))

fnDef :: Parser (FnDef 'Unchecked)
fnDef = do
    d <- fnDecl
    b <- block
    pure (FnDef d b)

fnDecl :: Parser (FnDecl 'Unchecked)
fnDecl = do
    "fn" *> space1
    n <- valueName
    as <- located (parenthesized (commaSeparated fnArg))
    "->" *> space
    t <- located typeName
    pure (UncheckedFnDecl n (fmap Vector.fromList as) t)

fnArg :: Parser (FnArg 'Unchecked)
fnArg = do
    an <- valueName
    colon
    at <- typeName
    pure (FnArg an at)

block :: Parser (Block 'Unchecked)
block = withinBraces $ do
    ss <- many (located (Parser.try statement))
    e <- located expr
    pure (UncheckedBlock (Vector.fromList ss) e)

statement :: Parser (Statement 'Unchecked)
statement = letStatement <|> exprStatement
  where
    letStatement = do
        "let" *> space1
        p <- pattern
        equal
        e <- expr
        semicolon
        pure (UncheckedLetStatement p e)

    exprStatement = fmap UncheckedExprStatement expr <* semicolon

pattern :: Parser (Pattern 'Unchecked)
pattern = varPattern <|> structPattern
  where
    varPattern = fmap VarPattern valueName

    structPattern = do
        n <- located structName
        vs <- located (parenthesized (commaSeparated valueName))
        pure (UncheckedStructPattern n (fmap Vector.fromList vs))

expr :: Parser (Expr 'Unchecked)
expr = fmap UncheckedExprWithBlock exprWithBlock <|> fmap UncheckedExprWithoutBlock (located exprWithoutBlock)

exprWithBlock :: Parser (ExprWithBlock 'Unchecked)
exprWithBlock = blockExpr <|> ifExpr <|> matchExpr
  where
    blockExpr = fmap BlockExpr block

    ifExpr = do
        "if" *> space1
        e <- located exprWithoutBlock
        b0 <- block
        "else" *> space1
        b1 <- block
        pure (UncheckedIfExpr e b0 b1)

    matchExpr = do
        "match" *> space1
        location <- fmap Location Parser.getOffset
        e <- located exprWithoutBlock
        as <- withinBraces (commaSeparated matchArm)
        pure (UncheckedMatchExpr location e as)

matchArm :: Parser (MatchArm 'Unchecked)
matchArm = do
    n <- located enumName
    void "::"
    v <- located enumVariantName
    xs <- located (parenthesized (commaSeparated valueName))
    "=>" *> space
    e <- expr
    pure (UncheckedMatchArm n v (fmap Vector.fromList xs) e)

exprWithoutBlock :: Parser (ExprWithoutBlock 'Unchecked)
exprWithoutBlock = makeExprParser term ops
  where
    term =
        Parser.try litExpr
        <|> parenthesized exprWithoutBlock
        <|> Parser.try callExpr
        <|> Parser.try printLnExpr
        <|> varExpr
        <|> Parser.try enumExpr
        <|> Parser.try structExpr
        <|> constExpr

    litExpr = fmap LitExpr lit

    callExpr = do
        n <- located valueName
        es <- located (parenthesized (commaSeparated exprWithoutBlock))
        pure (UncheckedCallExpr n (fmap Vector.fromList es))

    varExpr = fmap UncheckedVarExpr (located valueName)

    constExpr = fmap UncheckedConstExpr (located constName)

    enumExpr = do
        n <- located enumName
        void "::"
        v <- located enumVariantName
        es <- located (parenthesized (commaSeparated exprWithoutBlock))
        pure (UncheckedEnumExpr n v (fmap Vector.fromList es))

    structExpr = do
        n <- located structName
        es <- located (parenthesized (commaSeparated exprWithoutBlock))
        pure (UncheckedStructExpr n (fmap Vector.fromList es))

    printLnExpr = do
        void "println"
        Parser.char '!' *> space
        (f, es) <- parenthesized $ do
            f <- formatString
            es <- many (comma *> located exprWithoutBlock)
            pure (f, es)
        pure (UncheckedPrintLnExpr f es)
      where
        formatString = do
            void (Parser.char '"')
            cs <- many (located formatStringChunk)
            void (Parser.char '"')
            space
            pure (UncheckedFormatString cs)

        formatStringChunk = hole <|> nonHole

        hole = "{}" $> Hole

        nonHole = NonHole <$>
            Parser.takeWhile1P
                (Just "non-hole character")
                (\c -> isAscii c && isPrint c && c /= '"' && c /= '{')

    ops =
        [
            [
                Prefix (fmap (UncheckedUnaryOpExpr . ($> Neg)) (located (symbolic '-')))
            ],
            [
                Prefix (fmap (UncheckedUnaryOpExpr . ($> Not)) (located (symbolic '!')))
            ],
            [
                Postfix (fmap (UncheckedUnaryOpExpr . fmap As) (located (symbol "as" *> typeName)))
            ],
            [
                InfixL (fmap (UncheckedBinaryOpExpr . ($> Mul)) (located (symbolic '*'))),
                InfixL (fmap (UncheckedBinaryOpExpr . ($> Div)) (located (symbolic '/'))),
                InfixL (fmap (UncheckedBinaryOpExpr . ($> Rem)) (located (symbolic '%')))
            ],
            [
                InfixL (fmap (UncheckedBinaryOpExpr . ($> Add)) (located (symbolic '+'))),
                InfixL (fmap (UncheckedBinaryOpExpr . ($> Sub)) (located (symbolic '-')))
            ],
            [
                InfixL (fmap (UncheckedBinaryOpExpr . ($> Shl)) (located (symbol "<<"))),
                InfixL (fmap (UncheckedBinaryOpExpr . ($> Shr)) (located (symbol ">>")))
            ],
            [
                InfixL (fmap (UncheckedBinaryOpExpr . ($> BitAnd)) (located (Parser.try bitAnd)))
            ],
            [
                InfixL (fmap (UncheckedBinaryOpExpr . ($> BitXor)) (located (symbolic '^')))
            ],
            [
                InfixL (fmap (UncheckedBinaryOpExpr . ($> BitOr)) (located (Parser.try bitOr)))
            ],
            [
                InfixN (fmap (UncheckedBinaryOpExpr . ($> Cmp Eq)) (located (symbol "=="))),
                InfixN (fmap (UncheckedBinaryOpExpr . ($> Cmp Ne)) (located (symbol "!="))),
                InfixN (fmap (UncheckedBinaryOpExpr . ($> Cmp Le)) (located (symbol "<="))),
                InfixN (fmap (UncheckedBinaryOpExpr . ($> Cmp Ge)) (located (symbol ">="))),
                InfixN (fmap (UncheckedBinaryOpExpr . ($> Cmp Lt)) (located (symbolic '<'))),
                InfixN (fmap (UncheckedBinaryOpExpr . ($> Cmp Gt)) (located (symbolic '>')))
            ],
            [
                InfixL (fmap (UncheckedBinaryOpExpr . ($> And)) (located (symbol "&&")))
            ],
            [
                InfixL (fmap (UncheckedBinaryOpExpr . ($> Or)) (located (symbol "||")))
            ]
        ]
      where
        bitAnd = Parser.char '&' <* Parser.notFollowedBy (Parser.char '&') <* space

        bitOr = Parser.char '|' <* Parser.notFollowedBy (Parser.char '|') <* space

-- This definition ties down all the type variables.
decimal :: Parser Integer
decimal = Text.Megaparsec.Char.Lexer.decimal

lit :: Parser Lit
lit = unitLit <|> boolLit <|> Parser.try f64Lit <|> i64Lit
  where
    unitLit = fmap UnitLit ("()" $> ()) <* space

    boolLit = fmap BoolLit (true $> True <|> false $> False) <* space
      where
        true = "true" <* Parser.notFollowedBy (Parser.satisfy isIdentifierContinuation)

        false = "false" <* Parser.notFollowedBy (Parser.satisfy isIdentifierContinuation)

    f64Lit = do
        n <- decimal
        void (Parser.char '.')
        (s, c) <- Parser.match (Parser.option 0 decimal) <* space
        pure (F64Lit (encodeDecimalFloat n c (-Text.length s)))
      where
        encodeDecimalFloat n c e = fromIntegral n + fromIntegral c * 10 ** fromIntegral e

    i64Lit = fmap I64Lit decimal <* space

constDef :: Parser (ConstDef 'Unchecked)
constDef = do
    "const" *> space1
    x <- constName
    colon
    t <- typeName
    equal
    c <- located constInit
    semicolon
    pure (UncheckedConstDef (ConstDecl x t) c)
  where
    constInit = litInit <|> negLitInit <|> Parser.try structInit <|> enumInit

    litInit = fmap LitInit lit

    negLitInit = do
       Parser.char '-' *> space
       l <- located lit
       pure (UncheckedNegLitInit l)

    structInit = do
        n <- located structName
        cs <- located (parenthesized (commaSeparated constInit))
        pure (UncheckedStructInit n (fmap Vector.fromList cs))

    enumInit = do
        n <- located enumName
        void "::"
        v <- located enumVariantName
        cs <- located (parenthesized (commaSeparated constInit))
        pure (UncheckedEnumInit n v (fmap Vector.fromList cs))

isEndOfLine :: Char -> Bool
isEndOfLine c = c == '\n' || c == '\r'

comment :: Parser ()
comment = do
    void "//"
    void (Parser.takeWhileP (Just "any non-EOL character") (not . isEndOfLine))
    Parser.space

space :: Parser ()
space = Parser.space *> Parser.skipMany comment

space1 :: Parser ()
space1 = do
    Parser.space1 <|> comment
    Parser.skipMany comment

symbol :: Text -> Parser Text
symbol s = Parser.string s <* space

symbolic :: Char -> Parser Char
symbolic c = Parser.char c <* space

comma :: Parser ()
comma = Parser.char ',' *> space

commaSeparated :: Parser a -> Parser [a]
commaSeparated = (`Parser.sepBy` comma)

parenthesized :: Parser a -> Parser a
parenthesized p = do
    Parser.char '(' *> space
    result <- p
    Parser.char ')' *> space
    pure result

semicolon :: Parser ()
semicolon = Parser.char ';' *> space

colon :: Parser ()
colon = Parser.char ':' *> space

equal :: Parser ()
equal = Parser.char '=' *> space

withinBraces :: Parser a -> Parser a
withinBraces p = do
    Parser.char '{' *> space
    result <- p
    Parser.char '}' *> space
    pure result

isIdentifierContinuation :: Char -> Bool
isIdentifierContinuation c = isAscii c && (isAlphaNum c || c == '_')

identifierContinuation :: Parser Text
identifierContinuation =
    Parser.takeWhileP
        (Just "identifier continuation character")
        isIdentifierContinuation

lowerIdentifier :: Parser Text
lowerIdentifier = do
    a <- Parser.satisfy (\c -> isAsciiLower c || c == '_')
    s <- identifierContinuation
    space
    pure (Text.cons a s)

upperIdentifier :: Parser Text
upperIdentifier = do
    a <- Parser.satisfy isAsciiUpper
    s <- identifierContinuation
    space
    pure (Text.cons a s)

constNameContinuation :: Parser Text
constNameContinuation =
    Parser.takeWhileP
        (Just "constant name continuation character")
        (\c -> isAsciiUpper c || isDigit c || c == '_')

constName :: Parser Text
constName = do
    a <- Parser.satisfy (\c -> isAsciiUpper c || c == '_')
    s <- constNameContinuation
    space
    pure (Text.cons a s)

valueName :: Parser Text
valueName = lowerIdentifier

structName :: Parser Text
structName = upperIdentifier

enumName :: Parser Text
enumName = upperIdentifier

enumVariantName :: Parser Text
enumVariantName = upperIdentifier

typeName :: Parser (Type 'Unchecked)
typeName = unit <|> bool <|> i64 <|> f64 <|> structOrEnum
  where
    unit = symbol "()" $> Unit

    bool = symbol "bool" $> Bool

    i64 = symbol "i64" $> I64

    f64 = symbol "f64" $> F64

    structOrEnum = fmap StructOrEnum (located upperIdentifier)
