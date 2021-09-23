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
import Control.Monad.Combinators.Expr (
    Operator (InfixL, InfixN, Postfix, Prefix), makeExprParser)
import Data.Bifunctor (bimap)
import Data.Char (
    isAlphaNum, isAscii, isAsciiLower, isAsciiUpper, isDigit, isPrint)
import Data.Functor (($>), void)
import Data.Monoid (Endo (Endo, appEndo))
import Data.Text (Text)
import Data.Void (Void)
import Lens.Micro (_1, _2, _3, over)
import Text.Megaparsec (ParseErrorBundle, Parsec)
import Text.Megaparsec.Error (errorBundlePretty)

import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Text.Megaparsec as Parser
import qualified Text.Megaparsec.Char as Parser
import qualified Text.Megaparsec.Char.Lexer

import Coy.Syntax

type Parser = Parsec Void Text

newtype ParseError = ParseError (ParseErrorBundle Text Void)

instance Show ParseError where
    show (ParseError e) = errorBundlePretty e

cons :: a -> Endo [a]
cons = Endo . (:)

run :: Monoid m => Endo m -> m
run e = appEndo e mempty

parse :: String -> Text -> Either ParseError (Module 'Unchecked)
parse n s = do
    let result = Parser.parse p n s
    (typeDefs, constDefs, fnDefs) <- bimap ParseError runAll result
    pure (UncheckedModule typeDefs constDefs fnDefs)
  where
    -- Builds a triple of difference lists.
    p = space *> go mempty <* Parser.eof

    -- Corresponds to @many@.
    go e = go1 e <|> go2 e <|> go3 e <|> pure e

    -- Correspond to different incarnations of @some@.
    go1 e = do
        td <- typeDef
        go (over _1 (<> cons td) e)

    go2 e = do
        cd <- constDef
        go (over _2 (<> cons cd) e)

    go3 e = do
        fd <- fnDef
        go (over _3 (<> cons fd) e)

    -- Converts the difference lists to ordinary lists.
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
        ss <- many (Parser.try statement)
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
        (f, es) <- parenthesized (do
            f <- formatString
            es <- many (comma *> exprWithoutBlock)
            pure (f, es))
        pure (PrintLnExpr f es)
      where
        formatString = do
            void (Parser.char '"')
            cs <- many formatStringChunk
            void (Parser.char '"')
            space
            pure (UncheckedFormatString cs)

        formatStringChunk = hole <|> nonHole

        hole = "{}" $> Hole

        nonHole = fmap NonHole (
            Parser.takeWhile1P (Just "non-hole character")
                (\c -> isAscii c && isPrint c && c /= '"' && c /= '{'))

    ops =
        [
            [Prefix (symbolic '-' $> UnaryOpExpr Neg)],
            [Prefix (symbolic '!' $> UnaryOpExpr Not)],
            [Postfix (fmap (UnaryOpExpr . As) (symbol "as" *> typeName))],
            [
                InfixL (symbolic '*' $> BinaryOpExpr Mul),
                InfixL (symbolic '/' $> BinaryOpExpr Div),
                InfixL (symbolic '%' $> BinaryOpExpr Rem)
            ],
            [
                InfixL (symbolic '+' $> BinaryOpExpr Add),
                InfixL (symbolic '-' $> BinaryOpExpr Sub)
            ],
            [
                InfixL (symbol "<<" $> BinaryOpExpr Shl),
                InfixL (symbol ">>" $> BinaryOpExpr Shr)
            ],
            [InfixL (Parser.try bitAnd $> BinaryOpExpr BitAnd)],
            [InfixL (symbolic '^' $> BinaryOpExpr BitXor)],
            [InfixL (Parser.try bitOr $> BinaryOpExpr BitOr)],
            [
                InfixN (symbol "==" $> BinaryOpExpr (Cmp Eq)),
                InfixN (symbol "!=" $> BinaryOpExpr (Cmp Ne)),
                InfixN (symbol "<=" $> BinaryOpExpr (Cmp Le)),
                InfixN (symbol ">=" $> BinaryOpExpr (Cmp Ge)),
                InfixN (symbolic '<' $> BinaryOpExpr (Cmp Lt)),
                InfixN (symbolic '>' $> BinaryOpExpr (Cmp Gt))
            ],
            [InfixL (symbol "&&" $> BinaryOpExpr And)],
            [InfixL (symbol "||" $> BinaryOpExpr Or)]
        ]
      where
        bitAnd =
                Parser.char '&'
            <*  Parser.notFollowedBy (Parser.char '&')
            <*  space

        bitOr =
                Parser.char '|'
            <*  Parser.notFollowedBy (Parser.char '|')
            <*  space

-- This definition ties down all the type variables.
decimal :: Parser Integer
decimal = Text.Megaparsec.Char.Lexer.decimal

lit :: Parser Lit
lit = unitLit <|> boolLit <|> Parser.try f64Lit <|> i64Lit
  where
    unitLit = fmap UnitLit ("()" $> ()) <* space

    boolLit = fmap BoolLit (true $> True <|> false $> False) <* space
      where
        true =
                "true"
            <*  Parser.notFollowedBy (Parser.satisfy isIdentifierContinuation)

        false =
                "false"
            <*  Parser.notFollowedBy (Parser.satisfy isIdentifierContinuation)

    f64Lit = do
        n <- decimal
        void (Parser.char '.')
        (s, c) <- Parser.match (Parser.option 0 decimal) <* space
        pure (F64Lit (encodeDecimalFloat n c (-Text.length s)))
      where
        encodeDecimalFloat n c e =
            fromIntegral n + fromIntegral c * 10 ** fromIntegral e

    i64Lit = fmap I64Lit decimal <* space

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
    constInit = litInit <|> negLitInit <|> Parser.try structInit <|> enumInit

    litInit = fmap LitInit lit

    negLitInit = do
       Parser.char '-' *> space
       l <- lit
       pure (UncheckedNegLitInit l)

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
    Parser.takeWhileP (Just "identifier continuation character")
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
    Parser.takeWhileP (Just "constant name continuation character")
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

    structOrEnum = fmap StructOrEnum upperIdentifier
