{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Coy.Syntax where

import Data.Text (Text)
import Data.Vector (Vector)

data Status = Unchecked | Checked

data Module (u :: Status) where
    UncheckedModule
        :: [TypeDef 'Unchecked]
        -> [ConstDef 'Unchecked]
        -> [FnDef 'Unchecked]
        -> Module 'Unchecked

    CheckedModule
        :: [TypeDef 'Checked]
        -- ^ Struct and enum definitions.
        -> [ConstDef 'Checked]
        -- ^ Definitions of global constants.
        -> FnDef 'Checked
        -- ^ Definition of the main function.
        -> [FnDef 'Checked]
        -- ^ Definitions of the other functions.
        -> Module 'Checked

deriving instance Show (Module u)

data TypeDef (u :: Status)
    = StructDef Text (Vector (Type u))
    | EnumDef Text [EnumVariant u]
    deriving Show

typeDefName :: TypeDef u -> Text
typeDefName = \case
    StructDef n _ -> n
    EnumDef n _ -> n

data Type (u :: Status) where
    Unit :: Type u
    Bool :: Type u
    I64 :: Type u
    F64 :: Type u

    StructOrEnum :: Text -> Type 'Unchecked

    Struct :: Text -> Type 'Checked
    Enum :: Text -> Type 'Checked

deriving instance Eq (Type u)
deriving instance Ord (Type u)
deriving instance Show (Type u)

data EnumVariant (u :: Status) = EnumVariant Text (Vector (Type u))
    deriving Show

data FnDef (u :: Status) = FnDef (FnDecl u) (Block u)
    deriving Show

fnDefName :: FnDef u -> Text
fnDefName (FnDef (FnDecl n _ _) _) = n

data FnDecl (u :: Status) = FnDecl Text (Vector (FnArg u)) (Type u)
    deriving Show

data FnArg (u :: Status) = FnArg Text (Type u)
    deriving Show

fnArgType :: FnArg u -> Type u
fnArgType (FnArg _ at) = at

data Block (u :: Status) = Block (Vector (Statement u)) (Expr u)
    deriving Show

data Statement (u :: Status)
    = LetStatement (Pattern u) (Expr u)
    | ExprStatement (Expr u)
    deriving Show

data Pattern (u :: Status) where
    VarPattern :: Text -> Pattern u

    UncheckedStructPattern :: Text -> Vector Text -> Pattern 'Unchecked

    CheckedStructPattern :: Vector (Text, Type 'Checked) -> Pattern 'Checked

deriving instance Show (Pattern u)

data Expr (u :: Status)
    = ExprWithBlock (ExprWithBlock u)
    | ExprWithoutBlock (ExprWithoutBlock u)
    deriving Show

data ExprWithBlock (u :: Status) where
    BlockExpr :: Block u -> ExprWithBlock u
    IfExpr :: ExprWithoutBlock u -> Block u -> Block u -> ExprWithBlock u

    UncheckedMatchExpr
        :: ExprWithoutBlock 'Unchecked
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

deriving instance Show (ExprWithBlock u)

data MatchArm (u :: Status) where
    UncheckedMatchArm
        :: Text
        -- ^ Name of the enum.
        -> Text
        -- ^ Name of the enum variant.
        -> Vector Text
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

deriving instance Show (MatchArm u)

data ExprWithoutBlock (u :: Status) where
    LitExpr :: Lit -> ExprWithoutBlock u

    UncheckedVarExpr :: Text -> ExprWithoutBlock 'Unchecked

    CheckedVarExpr :: Text -> Type 'Checked -> ExprWithoutBlock 'Checked

    UncheckedConstExpr :: Text -> ExprWithoutBlock 'Unchecked

    CheckedConstExpr :: Text -> Type 'Checked -> ExprWithoutBlock 'Checked

    UnaryOpExpr
        :: UnaryOp u
        -- ^ The unary operator.
        -> ExprWithoutBlock u
        -- ^ Argument.
        -> ExprWithoutBlock u

    BinaryOpExpr
        :: BinaryOp u
        -- ^ The binary operator.
        -> ExprWithoutBlock u
        -- ^ First argument.
        -> ExprWithoutBlock u
        -- ^ Second argument.
        -> ExprWithoutBlock u

    UncheckedCallExpr
        :: Text
        -- ^ Name of the function.
        -> Vector (ExprWithoutBlock 'Unchecked)
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
        :: Text
        -- ^ Name of the struct.
        -> Vector (ExprWithoutBlock 'Unchecked)
        -- ^ Arguments.
        -> ExprWithoutBlock 'Unchecked

    CheckedStructExpr
        :: Text
        -- ^ Name of the struct.
        -> Vector (ExprWithoutBlock 'Checked, Type 'Checked)
        -- ^ Arguments with their types.
        -> ExprWithoutBlock 'Checked

    UncheckedEnumExpr
        :: Text
        -- ^ Name of the enum.
        -> Text
        -- ^ Name of the enum variant.
        -> Vector (ExprWithoutBlock 'Unchecked)
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

    PrintLnExpr
        :: FormatString u
        -- ^ The format string.
        -> [ExprWithoutBlock u]
        -- ^ Arguments.
        -> ExprWithoutBlock u

deriving instance Show (ExprWithoutBlock u)

data Lit
    = UnitLit ()
    | BoolLit Bool
    | I64Lit Integer
    | F64Lit Double
    deriving Show

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

deriving instance Show (BinaryOp u)

data Predicate
    = Eq
    | Ne
    | Lt
    | Gt
    | Le
    | Ge
    deriving Show

data FormatString (u :: Status) where
    UncheckedFormatString :: [FormatStringChunk] -> FormatString 'Unchecked

    CheckedFormatString :: Text -> FormatString 'Checked

deriving instance Show (FormatString u)

data FormatStringChunk = Hole | NonHole Text
    deriving (Eq, Ord, Show)

data ConstDef (u :: Status) = ConstDef (ConstDecl u) (ConstInit u)
    deriving Show

data ConstDecl (u :: Status) = ConstDecl Text (Type u)
    deriving Show

data ConstInit (u :: Status) where
    LitInit :: Lit -> ConstInit u

    UncheckedNegLitInit :: Lit -> ConstInit 'Unchecked

    NegI64LitInit :: Integer -> ConstInit 'Checked

    NegF64LitInit :: Double -> ConstInit 'Checked

    StructInit :: Text -> Vector (ConstInit u) -> ConstInit u

    UncheckedEnumInit
        :: Text
        -- ^ Name of the enum.
        -> Text
        -- ^ Name of the enum variant.
        -> Vector (ConstInit 'Unchecked)
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

deriving instance Show (ConstInit u)
