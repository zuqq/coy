{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Guiding principles:
--
-- - Every token owns the whitespace that comes after it.
-- - Every statement owns the semicolon that comes after it.
module Coy.Parse
    (
    -- * Error type
      ParseError (..)
    -- * Entry point
    , parse
    )
    where

import Control.Applicative ((<|>), many)
import Data.Attoparsec.Expr (
    Assoc (AssocLeft, AssocNone),
    Operator (Infix, Postfix, Prefix),
    buildExpressionParser)
import Data.Attoparsec.Text (Parser)
import Data.Bifunctor (bimap)
import Data.Char (isAlphaNum, isAscii, isAsciiLower, isAsciiUpper, isSpace)
import Data.Either (partitionEithers)
import Data.Functor (($>), void)
import Data.Text (Text)

import qualified Data.Attoparsec.Text as Parser
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Coy.Syntax

newtype ParseError = ParseError String
    deriving Show

parse :: Text -> Either ParseError (Module 'Unchecked)
parse s = do
    let result = Parser.parseOnly p s
    (typeDefs, fnDefs) <- bimap ParseError partitionEithers result
    pure (Module typeDefs fnDefs)
  where
    p = space *> many (Parser.eitherP typeDef fnDef) <* Parser.endOfInput

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
    as <- parenthesized (commaSeparated fnArg)
    "->" *> space
    t <- typeName
    pure (FnDecl n (Vector.fromList as) t)

fnArg :: Parser (FnArg 'Unchecked)
fnArg = do
    an <- valueName
    colon
    at <- typeName
    pure (FnArg an at)

block :: Parser (Block 'Unchecked)
block =
    withinBraces (do
        ss <- many statement
        e <- expr
        pure (Block (Vector.fromList ss) e))

statement :: Parser (Statement 'Unchecked)
statement = letStatement <|> exprStatement
  where
    letStatement = do
        "let" *> space1
        p <- pattern
        Parser.char '=' *> space
        e <- expr
        semicolon
        pure (LetStatement p e)

    exprStatement = fmap ExprStatement expr <* semicolon

pattern :: Parser (Pattern 'Unchecked)
pattern = varPattern <|> structPattern
  where
    varPattern = fmap VarPattern valueName

    structPattern = do
        n <- structName
        vs <- parenthesized (commaSeparated valueName)
        pure (StructPattern n (Vector.fromList vs))

expr :: Parser (Expr 'Unchecked)
expr =
    fmap ExprWithBlock exprWithBlock <|> fmap ExprWithoutBlock exprWithoutBlock

exprWithBlock :: Parser (ExprWithBlock 'Unchecked)
exprWithBlock = blockExpr <|> ifExpr <|> matchExpr
  where
    blockExpr = fmap BlockExpr block

    ifExpr = do
        "if" *> space1
        x <- valueName
        b0 <- block
        "else" *> space1
        b1 <- block
        pure (IfExpr x b0 b1)

    matchExpr = do
        "match" *> space1
        x <- valueName
        as <- withinBraces (commaSeparated matchArm)
        pure (UncheckedMatchExpr x as)

matchArm :: Parser (MatchArm 'Unchecked)
matchArm = do
    n <- enumName
    void "::"
    v <- enumVariantName
    xs <- parenthesized (commaSeparated valueName)
    "=>" *> space
    e <- expr
    pure (UncheckedMatchArm n v (Vector.fromList xs) e)

exprWithoutBlock :: Parser (ExprWithoutBlock 'Unchecked)
exprWithoutBlock = buildExpressionParser operators simpleExpr
  where
    operators =
        [
            [Prefix (symbolic '-' $> UnaryOpExpr Neg)],
            [Prefix (symbolic '!' $> UnaryOpExpr Not)],
            [Postfix (fmap (UnaryOpExpr . As) (symbol "as" *> typeName))],
            [
                Infix (symbolic '*' $> BinaryOpExpr Mul) AssocLeft,
                Infix (symbolic '/' $> BinaryOpExpr Div) AssocLeft,
                Infix (symbolic '%' $> BinaryOpExpr Rem) AssocLeft
            ],
            [
                Infix (symbolic '+' $> BinaryOpExpr Add) AssocLeft,
                Infix (symbolic '-' $> BinaryOpExpr Sub) AssocLeft
            ],
            [
                Infix (symbol "<<" $> BinaryOpExpr Shl) AssocLeft,
                Infix (symbol ">>" $> BinaryOpExpr Shr) AssocLeft
            ],
            [Infix (symbolic '&' $> BinaryOpExpr BitAnd) AssocLeft],
            [Infix (symbolic '^' $> BinaryOpExpr BitXor) AssocLeft],
            [Infix (symbolic '|' $> BinaryOpExpr BitOr) AssocLeft],
            [
                Infix (symbol "==" $> BinaryOpExpr (Cmp Eq)) AssocNone,
                Infix (symbol "!=" $> BinaryOpExpr (Cmp Ne)) AssocNone,
                Infix (symbolic '<' $> BinaryOpExpr (Cmp Lt)) AssocNone,
                Infix (symbolic '>' $> BinaryOpExpr (Cmp Gt)) AssocNone,
                Infix (symbol "<=" $> BinaryOpExpr (Cmp Le)) AssocNone,
                Infix (symbol ">=" $> BinaryOpExpr (Cmp Ge)) AssocNone
            ],
            [Infix (symbol "&&" $> BinaryOpExpr And) AssocLeft],
            [Infix (symbol "||" $> BinaryOpExpr Or) AssocLeft]
        ]

    simpleExpr =
            parenthesized exprWithoutBlock
        <|> litExpr
        -- Here @callExpr@ and @printLnExpr@ need to precede @varExpr@ because
        -- they parse suffixes of the latter.
        <|> callExpr
        <|> printLnExpr
        <|> varExpr
        -- Similary, @enumExpr@ needs to precede @structExpr@.
        <|> enumExpr
        <|> structExpr

    litExpr = fmap LitExpr lit

    callExpr = do
        x <- valueName
        es <- parenthesized (commaSeparated exprWithoutBlock)
        pure (CallExpr (Call x (Vector.fromList es)))

    varExpr = fmap VarExpr valueName

    enumExpr = do
        n <- enumName
        void "::"
        v <- enumVariantName
        es <- parenthesized (commaSeparated exprWithoutBlock)
        pure (EnumExpr n (EnumVariantName v) (Vector.fromList es))

    structExpr = do
        n <- structName
        es <- parenthesized (commaSeparated exprWithoutBlock)
        pure (StructExpr n (Vector.fromList es))

    printLnExpr = do
        void "println"
        Parser.char '!' *> space
        (cs, es) <- parenthesized (do
            cs <- formatString
            es <- many (comma *> exprWithoutBlock)
            pure (cs, es))
        pure (PrintLnExpr (UncheckedFormatString cs) es)
      where
        formatString = do
            void (Parser.char '"')
            cs <- many formatStringChunk
            void (Parser.char '"')
            space
            pure cs

        formatStringChunk = "{}" <|>
            Parser.takeWhile1 (\c -> isAscii c && (isAlphaNum c || c == ' '))

lit :: Parser Lit
lit = unitLit <|> boolLit <|> f64Lit <|> i64Lit
  where
    unitLit = fmap UnitLit ("()" $> ()) <* space

    boolLit = fmap BoolLit ("true" $> True <|> "false" $> False) <* space

    f64Lit = do
        n <- decimal
        void (Parser.char '.')
        (s, c) <- Parser.match (Parser.option 0 decimal) <* space
        pure (F64Lit (encodeDecimalFloat n c (-Text.length s)))
      where
        decimal = Parser.decimal @Integer

        encodeDecimalFloat n c e =
            fromIntegral n + fromIntegral c * 10 ** fromIntegral e

    i64Lit = fmap I64Lit Parser.decimal <* space

space :: Parser ()
space = Parser.skipSpace

space1 :: Parser ()
space1 = void (Parser.takeWhile1 isSpace)

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

withinBraces :: Parser a -> Parser a
withinBraces p = do
    Parser.char '{' *> space
    result <- p
    Parser.char '}' *> space
    pure result

lowerIdentifier :: Parser Text
lowerIdentifier = do
    a <- Parser.satisfy (\c -> isAsciiLower c || c == '_')
    s <- Parser.takeWhile (\c -> isAscii c && (isAlphaNum c || c == '_'))
    space
    pure (Text.cons a s)

upperIdentifier :: Parser Text
upperIdentifier = do
    a <- Parser.satisfy isAsciiUpper
    s <- Parser.takeWhile (\c -> isAscii c && (isAlphaNum c || c == '_'))
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

    structOrEnum = fmap StructOrEnum upperIdentifier
