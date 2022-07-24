{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Coy.Parse where

import Control.Applicative (optional, (<|>))
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixN, Postfix, Prefix), makeExprParser)
import Data.Bifunctor (first)
import Data.Char (isAlphaNum, isAscii, isAsciiLower, isAsciiUpper, isDigit, isPrint)
import Data.Functor (void, ($>))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, many, (<?>))

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
    moduleItems <- first errorBundlePretty (Parser.parse parseModule filePath input)

    let typeDefs = [d | TypeDefItem d <- moduleItems]

    let constDefs = [d | ConstDefItem d <- moduleItems]

    let fnDefs = [d | FnDefItem d <- moduleItems]

    pure (UncheckedModule typeDefs constDefs fnDefs)
  where
    parseModule = many parseModuleItem <* Parser.eof

    parseModuleItem =
        TypeDefItem <$> located typeDef
        <|> ConstDefItem <$> (located constDef <?> "constant definition")
        <|> FnDefItem <$> (located fnDef <?> "function definition")

type Parser = Parsec Void Text

located :: Parser a -> Parser (Located a)
located p = do
    location <- Location <$> Parser.getOffset
    result <- p
    pure (Located location result)

typeDef :: Parser (TypeDef 'Unchecked)
typeDef = (structDef <?> "struct definition") <|> (enumDef <?> "enum definition")
  where
    structDef = do
        "struct" *> space1
        n <- structName <?> "struct name"
        ts <- parenthesized (commaSeparated (typeName <?> "type name"))
        semicolon
        pure (StructDef n (Vector.fromList ts))

    enumDef = do
        "enum" *> space1
        n <- parseEnumName <?> "enum name"
        vs <- withinBraces (commaSeparated (located enumVariant))
        pure (UncheckedEnumDef n vs)

enumVariant :: Parser (EnumVariant 'Unchecked)
enumVariant = do
    v <- parseEnumVariantName <?> "enum variant name"
    ts <- parenthesized (commaSeparated (typeName <?> "type name"))
    pure (EnumVariant v (Vector.fromList ts))

fnDef :: Parser (FnDef 'Unchecked)
fnDef = do
    d <- fnDecl
    b <- block
    pure (FnDef d b)

fnDecl :: Parser (FnDecl 'Unchecked)
fnDecl = do
    "fn" *> space1
    n <- located valueName <?> "function name"
    as <- located (parenthesized (commaSeparated fnArg))
    "->" *> space
    t <- located typeName <?> "function return type"
    pure (UncheckedFnDecl n (Vector.fromList <$> as) t)

fnArg :: Parser (FnArg 'Unchecked)
fnArg = do
    an <- located valueName <?> "argument name"
    colon
    at <- typeName <?> "argument type"
    pure (UncheckedFnArg an at)

block :: Parser (Block 'Unchecked)
block = do
    (ss, e) <- withinBraces statements
    pure (UncheckedBlock (Vector.fromList ss) e)
  where
    statements = do
        s <- located statement
        case unpack s of
            UncheckedLetStatement _ _ -> semicolon *> loop s
            UncheckedExprStatement e -> do
                continue <- optional semicolon
                case continue of
                    Nothing -> pure (mempty, Located (locate s) e)
                    Just () -> loop s

    loop s = do
        (ss, e) <- statements
        pure (s : ss, e)

statement :: Parser (Statement 'Unchecked)
statement = (letStatement <?> "`let` statement") <|> (exprStatement <?> "expression")
  where
    letStatement = do
        Parser.try ("let" *> space1)
        p <- pattern
        equal
        e <- expr <?> "expression"
        pure (UncheckedLetStatement p e)

    exprStatement = UncheckedExprStatement <$> (expr <?> "expression")

pattern :: Parser (Pattern 'Unchecked)
pattern = (varPattern <?> "variable") <|> (structPattern <?> "struct pattern")
  where
    varPattern = UncheckedVarPattern <$> located valueName

    structPattern = do
        n <- located structName <?> "struct name"
        vs <- located (parenthesized (commaSeparated (located valueName <?> "variable")))
        pure (UncheckedStructPattern n (Vector.fromList <$> vs))

expr :: Parser (Expr 'Unchecked)
expr =
    UncheckedExprWithBlock <$> exprWithBlock
    <|> UncheckedExprWithoutBlock <$> located exprWithoutBlock

exprWithBlock :: Parser (ExprWithBlock 'Unchecked)
exprWithBlock =
    (blockExpr <?> "block expression")
    <|> (ifExpr <?> "`if` expression")
    <|> (matchExpr <?> "`match` expression")
  where
    blockExpr = BlockExpr <$> block

    ifExpr = do
        "if" *> space1
        e <- located exprWithoutBlock <?> "expression"
        b0 <- block
        "else" *> space1
        b1 <- block
        pure (UncheckedIfExpr e b0 b1)

    matchExpr = do
        "match" *> space1
        location <- Location <$> Parser.getOffset
        e <- located exprWithoutBlock <?> "expression"
        as <- withinBraces (commaSeparated (matchArm <?> "`match` arm"))
        pure (UncheckedMatchExpr location e as)

matchArm :: Parser (MatchArm 'Unchecked)
matchArm = do
    n <- located parseEnumName <?> "enum name"
    "::" *> space
    v <- located parseEnumVariantName <?> "enum variant name"
    xs <- located (parenthesized (commaSeparated (located valueName <?> "variable")))
    "=>" *> space
    e <- expr <?> "expression"
    pure (UncheckedMatchArm n v (Vector.fromList <$> xs) e)

exprWithoutBlock :: Parser (ExprWithoutBlock 'Unchecked)
exprWithoutBlock = makeExprParser term ops
  where
    term =
        (litExpr <?> "literal expression")
        <|> (parenthesized exprWithoutBlock <?> "parenthesized expression")
        <|> (callExpr <?> "call expression")
        <|> (printExpr <?> "`print!` expression")
        <|> (varExpr <?> "variable")
        <|> (enumExpr <?> "enum expression")
        <|> (structExpr <?> "struct expression")
        <|> (constExpr <?> "constant")

    litExpr = LitExpr <$> lit

    callExpr = do
        (n, location) <- Parser.try startCallExpr
        es <- commaSeparated (exprWithoutBlock <?> "function argument")
        Parser.char ')' *> space
        pure (UncheckedCallExpr n (Located location (Vector.fromList es)))
      where
        startCallExpr = do
            n <- located valueName <?> "function name"
            location <- Location <$> Parser.getOffset
            Parser.char '(' *> space
            pure (n, location)

    varExpr = UncheckedVarExpr <$> (located valueName <?> "variable")

    printExpr = do
        Parser.try ("print" *> space *> Parser.char '!' *> space *> Parser.char '(' *> space)
        f <- formatString <?> "format string"
        es <- many (comma *> (located exprWithoutBlock <?> "format string argument"))
        Parser.char ')' *> space
        pure (UncheckedPrintExpr f es)
      where
        formatString = do
            void (Parser.char '"')
            cs <- many (located formatStringChunk)
            void (Parser.char '"')
            space
            pure (UncheckedFormatString cs)

        formatStringChunk = nonHole <|> hole

        nonHole = NonHole <$> (escaped <|> text <|> leftBrace <|> rightBrace)

        escaped =
            "\\\"" $> "\""
            <|> "\\n" $> "\n"
            <|> "\\t" $> "\t"
            <|> "\\\\" $> "\\"
            <|> "\\0" $> "\0"

        text = escapePercentSign <$> Parser.takeWhile1P (Just "text character") isText

        escapePercentSign = Text.replace "%" "%%"

        isText c = isAscii c && isPrint c && c /= '"' && c /= '\\' && c /= '{' && c /= '}'

        leftBrace = "{{" $> "{"

        rightBrace = "}}" $> "}"

        hole = ("{" *> space *> "}") $> Hole

    enumExpr = do
        (n, v, location) <- Parser.try startEnumExpr
        es <- commaSeparated (exprWithoutBlock <?> "expression")
        Parser.char ')' *> space
        pure (UncheckedEnumExpr n v (Located location (Vector.fromList es)))
      where
        startEnumExpr = do
            n <- located parseEnumName <?> "enum name"
            "::" *> space
            v <- located parseEnumVariantName <?> "enum variant name"
            location <- Location <$> Parser.getOffset
            Parser.char '(' *> space
            pure (n, v, location)

    structExpr = do
        (n, location) <- Parser.try startStructExpr
        es <- commaSeparated (exprWithoutBlock <?> "expression")
        Parser.char ')' *> space
        pure (UncheckedStructExpr n (Located location (Vector.fromList es)))
      where
        startStructExpr = do
            n <- located structName <?> "struct name"
            location <- Location <$> Parser.getOffset
            Parser.char '(' *> space
            pure (n, location)

    constExpr = UncheckedConstExpr <$> (located constName <?> "constant")

    ops =
        [
            [
                Prefix (UncheckedUnaryOpExpr . ($> Neg) <$> located (symbol "-"))
            ],
            [
                Prefix (UncheckedUnaryOpExpr . ($> Not) <$> located (symbol "!"))
            ],
            [
                Postfix (UncheckedUnaryOpExpr . (As <$>) <$> located ("as" *> space1 *> typeName))
            ],
            [
                InfixL (UncheckedBinaryOpExpr . ($> Mul) <$> located (symbol "*")),
                InfixL (UncheckedBinaryOpExpr . ($> Div) <$> located (symbol "/")),
                InfixL (UncheckedBinaryOpExpr . ($> Rem) <$> located (symbol "%"))
            ],
            [
                InfixL (UncheckedBinaryOpExpr . ($> Add) <$> located (symbol "+")),
                InfixL (UncheckedBinaryOpExpr . ($> Sub) <$> located (symbol "-"))
            ],
            [
                InfixL (UncheckedBinaryOpExpr . ($> Shl) <$> located (symbol "<<")),
                InfixL (UncheckedBinaryOpExpr . ($> Shr) <$> located (symbol ">>"))
            ],
            [
                InfixL (UncheckedBinaryOpExpr . ($> BitAnd) <$> located bitAnd)
            ],
            [
                InfixL (UncheckedBinaryOpExpr . ($> BitXor) <$> located (symbol "^"))
            ],
            [
                InfixL (UncheckedBinaryOpExpr . ($> BitOr) <$> located bitOr)
            ],
            [
                InfixN (UncheckedBinaryOpExpr . ($> Cmp Eq) <$> located (symbol "==")),
                InfixN (UncheckedBinaryOpExpr . ($> Cmp Ne) <$> located (symbol "!=")),
                InfixN (UncheckedBinaryOpExpr . ($> Cmp Le) <$> located (symbol "<=")),
                InfixN (UncheckedBinaryOpExpr . ($> Cmp Ge) <$> located (symbol ">=")),
                InfixN (UncheckedBinaryOpExpr . ($> Cmp Lt) <$> located (symbol "<")),
                InfixN (UncheckedBinaryOpExpr . ($> Cmp Gt) <$> located (symbol ">"))
            ],
            [
                InfixL (UncheckedBinaryOpExpr . ($> And) <$> located (symbol "&&"))
            ],
            [
                InfixL (UncheckedBinaryOpExpr . ($> Or) <$> located (symbol "||"))
            ]
        ]
      where
        bitAnd = Parser.try (Parser.char '&' <* Parser.notFollowedBy (Parser.char '&') <* space)

        bitOr = Parser.try (Parser.char '|' <* Parser.notFollowedBy (Parser.char '|') <* space)

decimal :: Parser Integer
decimal = Text.Megaparsec.Char.Lexer.decimal

lit :: Parser Lit
lit =
    (unitLit <?> "`()` literal")
    <|> (boolLit <?> "`bool` literal")
    <|> (f64Lit <?> "`f64` literal")
    <|> (i64Lit <?> "`i64` literal")
  where
    unitLit = Parser.try (Parser.char '(' *> space *> Parser.char ')' *> space) $> UnitLit ()

    boolLit = BoolLit <$> (true $> True <|> false $> False) <* space
      where
        true = Parser.try ("true" <* Parser.notFollowedBy (Parser.satisfy isIdentifierContinuation))

        false = Parser.try ("false" <* Parser.notFollowedBy (Parser.satisfy isIdentifierContinuation))

    f64Lit = do
        n <- Parser.try (decimal <* Parser.char '.')
        (s, c) <- Parser.match (Parser.option 0 decimal) <* space
        pure (F64Lit (encodeDecimalFloat n c (-Text.length s)))
      where
        encodeDecimalFloat n c e = fromIntegral n + fromIntegral c * 10 ** fromIntegral e

    i64Lit = I64Lit <$> decimal <* space

constDef :: Parser (ConstDef 'Unchecked)
constDef = do
    "const" *> space1
    x <- constName <?> "constant name"
    colon
    t <- typeName <?> "constant type"
    equal
    c <- located constInit <?> "constant initializer"
    semicolon
    pure (UncheckedConstDef (ConstDecl x t) c)
  where
    constInit =
        (negLitInit <?> "negative literal")
        <|> (litInit <?> "literal")
        <|> (enumInit <?> "enum initializer")
        <|> (structInit <?> "struct initializer")

    negLitInit = do
       Parser.char '-' *> space
       l <- located lit
       pure (UncheckedNegLitInit l)

    litInit = LitInit <$> lit

    enumInit = do
        (n, v, location) <- Parser.try startEnumInit
        cs <- commaSeparated (constInit <?> "constant initializer")
        Parser.char ')' *> space
        pure (UncheckedEnumInit n v (Located location (Vector.fromList cs)))
      where
        startEnumInit = do
            n <- located parseEnumName <?> "enum name"
            "::" *> space
            v <- located parseEnumVariantName <?> "enum variant name"
            location <- Location <$> Parser.getOffset
            Parser.char '(' *> space
            pure (n, v, location)

    structInit = do
        (n, location) <- Parser.try startStructInit
        cs <- commaSeparated (constInit <?> "constant initializer")
        Parser.char ')' *> space
        pure (UncheckedStructInit n (Located location (Vector.fromList cs)))
      where
        startStructInit = do
            n <- located structName <?> "struct name"
            location <- Location <$> Parser.getOffset
            Parser.char '(' *> space
            pure (n, location)

isEndOfLine :: Char -> Bool
isEndOfLine c = c == '\n' || c == '\r'

comment :: Parser ()
comment = Parser.hidden ("//" *> Parser.takeWhileP Nothing (not . isEndOfLine) *> Parser.space)

space :: Parser ()
space = Parser.hidden (Parser.space *> Parser.skipMany comment)

space1 :: Parser ()
space1 = Parser.hidden ((Parser.space1 <|> comment) *> Parser.skipMany comment)

symbol :: Text -> Parser Text
symbol s = Parser.string s <* space

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
identifierContinuation = Parser.takeWhileP Nothing isIdentifierContinuation

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
constNameContinuation = Parser.takeWhileP Nothing (\c -> isAsciiUpper c || isDigit c || c == '_')

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

parseEnumName :: Parser Text
parseEnumName = upperIdentifier

parseEnumVariantName :: Parser Text
parseEnumVariantName = upperIdentifier

typeName :: Parser (Type 'Unchecked)
typeName = unit <|> bool <|> i64 <|> f64 <|> structOrEnum
  where
    unit = (Parser.char '(' *> space *> Parser.char ')' *> space) $> Unit

    bool = symbol "bool" $> Bool

    i64 = symbol "i64" $> I64

    f64 = symbol "f64" $> F64

    structOrEnum = StructOrEnum <$> located upperIdentifier
