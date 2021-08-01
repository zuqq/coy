{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Data.Char (
    isAlphaNum, isAscii, isAsciiLower, isAsciiUpper, isDigit, isPrint, isSpace)
import Data.Functor (($>), void)
import Lens.Micro (_1, _2, _3, over)
import Data.Monoid (Endo (Endo, appEndo))
import Data.Text (Text)

import qualified Data.Attoparsec.Text as Parser
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Coy.Syntax

newtype ParseError = ParseError String
    deriving Show

cons :: a -> Endo [a]
cons = Endo . (:)

run :: Monoid m => Endo m -> m
run e = appEndo e mempty

parse :: Text -> Either ParseError (Module 'Unchecked)
parse s = do
    let result = Parser.parseOnly p s
    (typeDefs, constDefs, fnDefs) <- bimap ParseError runAll result
    pure (UncheckedModule typeDefs constDefs fnDefs)
  where
    -- Builds a triple of difference lists.
    p = space *> go mempty <* Parser.endOfInput

    -- Corresponds to @many@.
    go e = go1 e <|> go2 e <|> go3 e <|> pure e

    -- Corresponds to different incarnations of @some@.
    go1 e = do
        td <- typeDef
        go (over _1 (<> cons td) e)

    go2 e = do
        cd <- constDef
        go (over _2 (<> cons cd) e)

    go3 e = do
        fd <- fnDef
        go (over _3 (<> cons fd) e)

    runAll = over _1 run . over _2 run . over _3 run

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
        equal
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
        pure (UncheckedStructPattern n (Vector.fromList vs))

expr :: Parser (Expr 'Unchecked)
expr =
    fmap ExprWithBlock exprWithBlock <|> fmap ExprWithoutBlock exprWithoutBlock

exprWithBlock :: Parser (ExprWithBlock 'Unchecked)
exprWithBlock = blockExpr <|> ifExpr <|> matchExpr
  where
    blockExpr = fmap BlockExpr block

    ifExpr = do
        "if" *> space1
        e <- exprWithoutBlock
        b0 <- block
        "else" *> space1
        b1 <- block
        pure (IfExpr e b0 b1)

    matchExpr = do
        "match" *> space1
        e <- exprWithoutBlock
        as <- withinBraces (commaSeparated matchArm)
        pure (UncheckedMatchExpr e as)

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
                Infix (symbol "<=" $> BinaryOpExpr (Cmp Le)) AssocNone,
                Infix (symbol ">=" $> BinaryOpExpr (Cmp Ge)) AssocNone,
                Infix (symbolic '<' $> BinaryOpExpr (Cmp Lt)) AssocNone,
                Infix (symbolic '>' $> BinaryOpExpr (Cmp Gt)) AssocNone
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
        -- Similary, @enumExpr@ needs to precede @structExpr@; both of them
        -- need to precede @constExpr@.
        <|> enumExpr
        <|> structExpr
        <|> constExpr

    litExpr = fmap LitExpr lit

    callExpr = do
        n <- valueName
        es <- parenthesized (commaSeparated exprWithoutBlock)
        pure (UncheckedCallExpr n (Vector.fromList es))

    varExpr = fmap UncheckedVarExpr valueName

    constExpr = fmap UncheckedConstExpr constName

    enumExpr = do
        n <- enumName
        void "::"
        v <- enumVariantName
        es <- parenthesized (commaSeparated exprWithoutBlock)
        pure (UncheckedEnumExpr n v (Vector.fromList es))

    structExpr = do
        n <- structName
        es <- parenthesized (commaSeparated exprWithoutBlock)
        pure (UncheckedStructExpr n (Vector.fromList es))

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

        formatStringChunk = "{}" <|> formatStringChunkNonHole

        formatStringChunkNonHole =
            Parser.takeWhile1
                (\c -> isAscii c && isPrint c && c /= '"' && c /= '{')

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

constDef :: Parser (ConstDef 'Unchecked)
constDef = do
    "const" *> space1
    x <- constName
    colon
    t <- typeName
    equal
    c <- constInit
    semicolon
    pure (ConstDef (ConstDecl x t) c)
  where
    constInit = litInit <|> structInit <|> enumInit

    litInit = fmap LitInit lit

    structInit = do
        n <- structName
        cs <- parenthesized (commaSeparated constInit)
        pure (StructInit n (Vector.fromList cs))

    enumInit = do
        n <- enumName
        void "::"
        v <- enumVariantName
        cs <- parenthesized (commaSeparated constInit)
        pure (UncheckedEnumInit n v (Vector.fromList cs))

comment :: Parser ()
comment = do
    void "//"
    Parser.skipWhile (not . Parser.isEndOfLine)
    Parser.skipSpace

space :: Parser ()
space = Parser.skipSpace *> Parser.skipMany comment

space1 :: Parser ()
space1 = do
    void (Parser.takeWhile1 isSpace) <|> comment
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

constName :: Parser Text
constName = do
    a <- Parser.satisfy (\c -> isAsciiUpper c || c == '_')
    s <- Parser.takeWhile (\c -> isAsciiUpper c || isDigit c || c == '_')
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
