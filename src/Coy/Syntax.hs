{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Coy.Syntax where

import Data.Text (Text)
import Data.Vector (Vector)

data Status = Unchecked | Checked

data Module (u :: Status) where
    UncheckedModule
        :: [TypeDef 'Unchecked]
        -> [FnDef 'Unchecked]
        -> Module 'Unchecked

    CheckedModule
        :: [TypeDef 'Checked]
        -- ^ Struct and enum definitions.
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

data Block (u :: Status) = Block (Vector (Statement u)) (Expr u)
    deriving Show

data Statement (u :: Status)
    = LetStatement (Pattern u) (Expr u)
    | ExprStatement (Expr u)
    deriving Show

data Pattern (u :: Status) = VarPattern Text | StructPattern Text (Vector Text)
    deriving Show

data Expr (u :: Status)
    = ExprWithBlock (ExprWithBlock u)
    | ExprWithoutBlock (ExprWithoutBlock u)
    deriving Show

data ExprWithBlock (u :: Status) where
    BlockExpr :: Block u -> ExprWithBlock u
    IfExpr :: Text -> Block u -> Block u -> ExprWithBlock u

    UncheckedMatchExpr
        :: Text
        -- ^ Name of the scrutinee.
        -> [MatchArm 'Unchecked]
        -- ^ Match arms.
        -> ExprWithBlock 'Unchecked

    CheckedMatchExpr
        :: Text
        -- ^ Name of the scrutinee.
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
        :: Vector Text
        -- ^ Names for the components.
        -> Expr 'Checked
        -- ^ Right-hand side of the match arm.
        -> MatchArm 'Checked

deriving instance Show (MatchArm u)

data ExprWithoutBlock (u :: Status)
    = LitExpr Lit
    | VarExpr Text
    | UnaryOpExpr (UnaryOp u) (ExprWithoutBlock u)
    | BinaryOpExpr (BinaryOp u) (ExprWithoutBlock u) (ExprWithoutBlock u)
    | CallExpr (Call u)
    | StructExpr Text (Vector (ExprWithoutBlock u))
    | EnumExpr Text (EnumVariantAccessor u) (Vector (ExprWithoutBlock u))
    | PrintLnExpr (FormatString u) [ExprWithoutBlock u]
    deriving Show

data Lit
    = UnitLit ()
    | BoolLit Bool
    | I64Lit Integer
    | F64Lit Double
    deriving Show

data UnaryOp (u :: Status) where
    -- @-x@
    Neg :: UnaryOp u
    FNeg :: UnaryOp 'Checked
    -- @!x@
    Not :: UnaryOp u
    -- @as@
    As :: Type 'Unchecked -> UnaryOp 'Unchecked
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

data Call (u :: Status) = Call Text (Vector (ExprWithoutBlock u))
    deriving Show

data EnumVariantAccessor (u :: Status) where
    EnumVariantName :: Text -> EnumVariantAccessor 'Unchecked

    EnumVariantIndex :: Int -> EnumVariantAccessor 'Checked

deriving instance Show (EnumVariantAccessor u)

data FormatString (u :: Status) where
    UncheckedFormatString :: [Text] -> FormatString 'Unchecked

    CheckedFormatString :: Text -> FormatString 'Checked

deriving instance Show (FormatString u)
