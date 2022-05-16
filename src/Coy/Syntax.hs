{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Coy.Syntax where

import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Text as Text

data Status = Unchecked | Checked

newtype Location = Location {offset :: Int}
    deriving (Eq, Ord, Show)

data Located a = Located {locate :: Location, unpack :: a}
    deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

data Module (u :: Status) where
    UncheckedModule
        :: [Located (TypeDef 'Unchecked)]
        -> [Located (ConstDef 'Unchecked)]
        -> [Located (FnDef 'Unchecked)]
        -> Module 'Unchecked

    CheckedModule
        :: [TypeDef 'Checked]
        -- ^ Struct and enum definitions.
        -> [ConstDef 'Checked]
        -- ^ Definitions of global constants.
        -> Vector Text
        -- ^ Intern pool.
        -> [FnDef 'Checked]
        -- ^ Definitions of the other functions.
        -> FnDef 'Checked
        -- ^ Definition of the main function.
        -> Module 'Checked

deriving instance Show (Module u)

data TypeDef (u :: Status)
    = StructDef Text (Vector (Type u))
    | EnumDef Text [EnumVariant u]
    deriving (Eq, Ord, Show)

typeDefName :: TypeDef u -> Text
typeDefName = \case
    StructDef n _ -> n
    EnumDef n _ -> n

data Type (u :: Status) where
    Unit :: Type u
    Bool :: Type u
    I64 :: Type u
    F64 :: Type u

    StructOrEnum :: Located Text -> Type 'Unchecked

    Struct :: Text -> Type 'Checked
    Enum :: Text -> Type 'Checked

prettyType :: Type u -> String
prettyType = \case
    Unit -> "()"
    Bool -> "bool"
    I64 -> "i64"
    F64 -> "f64"
    StructOrEnum n -> Text.unpack (unpack n)
    Struct n -> Text.unpack n
    Enum n -> Text.unpack n

deriving instance Eq (Type u)
deriving instance Ord (Type u)
deriving instance Show (Type u)

data EnumVariant (u :: Status) = EnumVariant Text (Vector (Type u))
    deriving (Eq, Ord, Show)

data FnDef (u :: Status) = FnDef (FnDecl u) (Block u)
    deriving (Eq, Ord, Show)

fnDefName :: FnDef u -> Text
fnDefName (FnDef d _) = fnDeclName d

data FnDecl (u :: Status) where
    UncheckedFnDecl :: Text -> Located (Vector (FnArg 'Unchecked)) -> Located (Type 'Unchecked) -> FnDecl 'Unchecked

    CheckedFnDecl :: Text -> Vector (FnArg 'Checked) -> Type 'Checked -> FnDecl 'Checked

fnDeclName :: FnDecl u -> Text
fnDeclName = \case
    UncheckedFnDecl n _ _ -> n
    CheckedFnDecl n _ _ -> n

deriving instance Eq (FnDecl u)
deriving instance Ord (FnDecl u)
deriving instance Show (FnDecl u)

data FnArg (u :: Status) = FnArg Text (Type u)
    deriving (Eq, Ord, Show)

fnArgType :: FnArg u -> Type u
fnArgType (FnArg _ at) = at

data Block (u :: Status) where
    UncheckedBlock :: Vector (Located (Statement 'Unchecked)) -> Located (Expr 'Unchecked) -> Block 'Unchecked

    CheckedBlock :: Vector (Statement 'Checked) -> Expr 'Checked -> Block 'Checked

deriving instance Eq (Block u)
deriving instance Ord (Block u)
deriving instance Show (Block u)

locateBlock :: Block 'Unchecked -> Location
locateBlock (UncheckedBlock _ e) = locate e

data Statement (u :: Status) where
    UncheckedLetStatement :: Pattern 'Unchecked -> Expr 'Unchecked -> Statement 'Unchecked

    CheckedLetStatement :: Pattern 'Checked -> Expr 'Checked -> Statement 'Checked

    UncheckedExprStatement :: Expr 'Unchecked -> Statement 'Unchecked

    CheckedExprStatement :: Expr 'Checked -> Statement 'Checked

deriving instance Eq (Statement u)
deriving instance Ord (Statement u)
deriving instance Show (Statement u)

data Pattern (u :: Status) where
    VarPattern :: Text -> Pattern u

    UncheckedStructPattern :: Located Text -> Located (Vector Text) -> Pattern 'Unchecked

    CheckedStructPattern :: Vector (Text, Type 'Checked) -> Pattern 'Checked

deriving instance Eq (Pattern u)
deriving instance Ord (Pattern u)
deriving instance Show (Pattern u)

data Expr (u :: Status) where
    UncheckedExprWithBlock :: ExprWithBlock 'Unchecked -> Expr 'Unchecked

    CheckedExprWithBlock :: ExprWithBlock 'Checked -> Expr 'Checked

    UncheckedExprWithoutBlock :: Located (ExprWithoutBlock 'Unchecked) -> Expr 'Unchecked

    CheckedExprWithoutBlock :: ExprWithoutBlock 'Checked -> Expr 'Checked

deriving instance Eq (Expr u)
deriving instance Ord (Expr u)
deriving instance Show (Expr u)

locateExpr :: Expr 'Unchecked -> Location
locateExpr = \case
    UncheckedExprWithBlock e -> locateExprWithBlock e
    UncheckedExprWithoutBlock e -> locate e

data ExprWithBlock (u :: Status) where
    BlockExpr :: Block u -> ExprWithBlock u

    UncheckedIfExpr
        :: Located (ExprWithoutBlock 'Unchecked)
        -> Block 'Unchecked
        -> Block 'Unchecked
        -> ExprWithBlock 'Unchecked

    CheckedIfExpr
        :: ExprWithoutBlock 'Checked
        -> Block 'Checked
        -> Block 'Checked
        -> ExprWithBlock 'Checked

    UncheckedMatchExpr
        :: Location
        -- ^ Start.
        -> Located (ExprWithoutBlock 'Unchecked)
        -- ^ Scrutinee.
        -> [MatchArm 'Unchecked]
        -- ^ Match arms.
        -> ExprWithBlock 'Unchecked

    CheckedMatchExpr
        :: ExprWithoutBlock 'Checked
        -- ^ Scrutinee.
        -> Text
        -- ^ Name of the enum.
        -> [MatchArm 'Checked]
        -- ^ Match arms, ordered by variant.
        -> ExprWithBlock 'Checked

-- Special treatment for expressions that are wrapped in a block, in order to
-- point at the expression that determines the blocks' type, not the block
-- itself.
locateExprWithBlock :: ExprWithBlock 'Unchecked -> Location
locateExprWithBlock = \case
    BlockExpr b -> locateBlock b
    UncheckedIfExpr _ b0 _ -> locateBlock b0
    UncheckedMatchExpr location _ [] -> location
    UncheckedMatchExpr _ _ (UncheckedMatchArm n _ _ _ : _) -> locate n

deriving instance Eq (ExprWithBlock u)
deriving instance Ord (ExprWithBlock u)
deriving instance Show (ExprWithBlock u)

data MatchArm (u :: Status) where
    UncheckedMatchArm
        :: Located Text
        -- ^ Name of the enum.
        -> Located Text
        -- ^ Name of the enum variant.
        -> Located (Vector Text)
        -- ^ Names for the components.
        -> Expr 'Unchecked
        -- ^ Right-hand side of the match arm.
        -> MatchArm 'Unchecked

    CheckedMatchArm
        :: Vector (Text, Type 'Checked)
        -- ^ Names for the components with their types.
        -> Expr 'Checked
        -- ^ Right-hand side of the match arm.
        -> MatchArm 'Checked

deriving instance Eq (MatchArm u)
deriving instance Ord (MatchArm u)
deriving instance Show (MatchArm u)

data ExprWithoutBlock (u :: Status) where
    LitExpr :: Lit -> ExprWithoutBlock u

    UncheckedVarExpr :: Located Text -> ExprWithoutBlock 'Unchecked

    CheckedVarExpr :: Text -> Type 'Checked -> ExprWithoutBlock 'Checked

    UncheckedConstExpr :: Located Text -> ExprWithoutBlock 'Unchecked

    CheckedConstExpr :: Text -> Type 'Checked -> ExprWithoutBlock 'Checked

    UncheckedUnaryOpExpr
        :: Located (UnaryOp 'Unchecked)
        -- ^ The unary operator.
        -> ExprWithoutBlock 'Unchecked
        -- ^ Argument.
        -> ExprWithoutBlock 'Unchecked

    CheckedUnaryOpExpr
        :: UnaryOp 'Checked
        -- ^ The unary operator.
        -> ExprWithoutBlock 'Checked
        -- ^ Argument.
        -> ExprWithoutBlock 'Checked

    UncheckedBinaryOpExpr
        :: Located (BinaryOp 'Unchecked)
        -- ^ The binary operator.
        -> ExprWithoutBlock 'Unchecked
        -- ^ First argument.
        -> ExprWithoutBlock 'Unchecked
        -- ^ Second argument.
        -> ExprWithoutBlock 'Unchecked

    CheckedBinaryOpExpr
        :: BinaryOp 'Checked
        -- ^ The binary operator.
        -> ExprWithoutBlock 'Checked
        -- ^ First argument.
        -> ExprWithoutBlock 'Checked
        -- ^ Second argument.
        -> ExprWithoutBlock 'Checked

    UncheckedCallExpr
        :: Located Text
        -- ^ Name of the function.
        -> Located (Vector (ExprWithoutBlock 'Unchecked))
        -- ^ Arguments.
        -> ExprWithoutBlock 'Unchecked

    CheckedCallExpr
        :: Text
        -- ^ Name of the function.
        -> Vector (ExprWithoutBlock 'Checked)
        -- ^ Arguments.
        -> Type 'Checked
        -- ^ Return type.
        -> ExprWithoutBlock 'Checked

    UncheckedStructExpr
        :: Located Text
        -- ^ Name of the struct.
        -> Located (Vector (ExprWithoutBlock 'Unchecked))
        -- ^ Arguments.
        -> ExprWithoutBlock 'Unchecked

    CheckedStructExpr
        :: Text
        -- ^ Name of the struct.
        -> Vector (ExprWithoutBlock 'Checked, Type 'Checked)
        -- ^ Arguments with their types.
        -> ExprWithoutBlock 'Checked

    UncheckedEnumExpr
        :: Located Text
        -- ^ Name of the enum.
        -> Located Text
        -- ^ Name of the enum variant.
        -> Located (Vector (ExprWithoutBlock 'Unchecked))
        -- ^ Arguments.
        -> ExprWithoutBlock 'Unchecked

    CheckedEnumExpr
        :: Text
        -- ^ Name of the enum.
        -> Int
        -- ^ Index of the enum variant.
        -> Vector (ExprWithoutBlock 'Checked, Type 'Checked)
        -- ^ Arguments with their types.
        -> ExprWithoutBlock 'Checked

    UncheckedPrintLnExpr
        :: FormatString 'Unchecked
        -- ^ The format string.
        -> [Located (ExprWithoutBlock 'Unchecked)]
        -- ^ Arguments.
        -> ExprWithoutBlock 'Unchecked

    CheckedPrintLnExpr
        :: FormatString 'Checked
        -- ^ The format string.
        -> [ExprWithoutBlock 'Checked]
        -- ^ Arguments.
        -> ExprWithoutBlock 'Checked

deriving instance Eq (ExprWithoutBlock u)
deriving instance Ord (ExprWithoutBlock u)
deriving instance Show (ExprWithoutBlock u)

data Lit
    = UnitLit ()
    | BoolLit Bool
    | I64Lit Integer
    | F64Lit Double
    deriving (Eq, Ord, Show)

litType :: Lit -> Type u
litType = \case
    UnitLit _ -> Unit
    BoolLit _ -> Bool
    I64Lit _ -> I64
    F64Lit _ -> F64

data UnaryOp (u :: Status) where
    -- @-x@
    Neg :: UnaryOp u
    FNeg :: UnaryOp 'Checked
    -- @!x@
    Not :: UnaryOp u
    -- @as@
    As :: Type 'Unchecked -> UnaryOp 'Unchecked
    AsF64 :: UnaryOp 'Checked
    AsI64 :: UnaryOp 'Checked

prettyUnaryOp :: UnaryOp u -> String
prettyUnaryOp = \case
    Neg -> "-"
    FNeg -> "-"
    Not -> "!"
    As t -> "as " <> prettyType t
    AsF64 -> "as f64"
    AsI64 -> "as i64"

deriving instance Eq (UnaryOp u)
deriving instance Ord (UnaryOp u)
deriving instance Show (UnaryOp u)

data BinaryOp (u :: Status) where
    -- Left-associative: @x * y@, @x / y@, @x % y@
    Mul :: BinaryOp u
    FMul :: BinaryOp 'Checked
    Div :: BinaryOp u
    FDiv :: BinaryOp 'Checked
    Rem :: BinaryOp u
    FRem :: BinaryOp 'Checked
    -- Left-associative: @x + y@, @x - y@
    Add :: BinaryOp u
    FAdd :: BinaryOp 'Checked
    Sub :: BinaryOp u
    FSub :: BinaryOp 'Checked
    -- Left-associative: @x << y@, @x >> y@
    Shl :: BinaryOp u
    Shr :: BinaryOp u
    -- Left-associative: @x & y@
    BitAnd :: BinaryOp u
    -- Left-associative: @x ^ y@
    BitXor :: BinaryOp u
    -- Left-associative: @x | y@
    BitOr :: BinaryOp u
    -- Non-associative: @x == y@, @x != y@, @x < y@, @x > y@, @x <= y@, @x >= y@
    Cmp :: Predicate -> BinaryOp 'Unchecked
    Icmp :: Predicate -> BinaryOp 'Checked
    Fcmp :: Predicate -> BinaryOp 'Checked
    -- Left-associative: @x && y@
    And :: BinaryOp u
    -- Left-associative: @x || y@
    Or :: BinaryOp u

prettyBinaryOp :: BinaryOp u -> String
prettyBinaryOp = \case
    Mul -> "*"
    FMul -> "*"
    Div -> "/"
    FDiv -> "/"
    Rem -> "%"
    FRem -> "%"
    Add -> "+"
    FAdd -> "+"
    Sub -> "-"
    FSub -> "-"
    Shl -> "<<"
    Shr -> ">>"
    BitAnd -> "&"
    BitXor -> "^"
    BitOr -> "|"
    Cmp p -> prettyPredicate p
    Icmp p -> prettyPredicate p
    Fcmp p -> prettyPredicate p
    And -> "&&"
    Or -> "||"

deriving instance Eq (BinaryOp u)
deriving instance Ord (BinaryOp u)
deriving instance Show (BinaryOp u)

data Predicate
    = Eq
    | Ne
    | Lt
    | Gt
    | Le
    | Ge
    deriving (Eq, Ord, Show)

prettyPredicate :: Predicate -> String
prettyPredicate = \case
    Eq -> "=="
    Ne -> "!="
    Lt -> "<"
    Gt -> ">"
    Le -> "<="
    Ge -> ">="

data FormatString (u :: Status) where
    UncheckedFormatString :: [Located FormatStringChunk] -> FormatString 'Unchecked

    CheckedFormatString :: Int -> FormatString 'Checked

deriving instance Eq (FormatString u)
deriving instance Ord (FormatString u)
deriving instance Show (FormatString u)

data FormatStringChunk = Hole | NonHole Text
    deriving (Eq, Ord, Show)

data ConstDef (u :: Status) where
    UncheckedConstDef :: ConstDecl 'Unchecked -> Located (ConstInit 'Unchecked) -> ConstDef 'Unchecked

    CheckedConstDef :: ConstDecl 'Checked -> ConstInit 'Checked -> ConstDef 'Checked

deriving instance Eq (ConstDef u)
deriving instance Ord (ConstDef u)
deriving instance Show (ConstDef u)

data ConstDecl (u :: Status) = ConstDecl Text (Type u)
    deriving (Eq, Ord, Show)

data ConstInit (u :: Status) where
    LitInit :: Lit -> ConstInit u

    UncheckedNegLitInit :: Located Lit -> ConstInit 'Unchecked

    NegI64LitInit :: Integer -> ConstInit 'Checked

    NegF64LitInit :: Double -> ConstInit 'Checked

    UncheckedStructInit :: Located Text -> Located (Vector (ConstInit 'Unchecked)) -> ConstInit 'Unchecked

    CheckedStructInit :: Text -> Vector (ConstInit 'Checked) -> ConstInit 'Checked

    UncheckedEnumInit
        :: Located Text
        -- ^ Name of the enum.
        -> Located Text
        -- ^ Name of the enum variant.
        -> Located (Vector (ConstInit 'Unchecked))
        -- ^ Values for the components.
        -> ConstInit 'Unchecked

    CheckedEnumInit
        :: Text
        -- ^ Name of the enum.
        -> Int
        -- ^ Index of the enum variant.
        -> Vector (ConstInit 'Checked)
        -- ^ Values for the components.
        -> ConstInit 'Checked

deriving instance Eq (ConstInit u)
deriving instance Ord (ConstInit u)
deriving instance Show (ConstInit u)
