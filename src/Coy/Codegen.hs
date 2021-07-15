{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Coy.Codegen (buildModule, codegen) where

import Control.Monad (void, zipWithM_)
import Control.Monad.State.Class (MonadState)
-- We need to use lazy @State@ for @-XRecursiveDo@.
import Control.Monad.Trans.State (State, evalState)
import Data.ByteString.Short (ShortByteString)
import Data.Foldable (foldl', for_, toList, traverse_)
import Data.List.Index (ifor, ifor_)
import Data.Map (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Lens.Micro (Lens', lens)
import Lens.Micro.Mtl ((%=), (.=), (<<%=), use)

import qualified Data.ByteString.Short as ByteString.Short
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Vector as Vector
import qualified LLVM.AST
import qualified LLVM.AST.Constant
import qualified LLVM.AST.IntegerPredicate
import qualified LLVM.AST.FloatingPointPredicate
import qualified LLVM.AST.Type
import qualified LLVM.IRBuilder

import Coy.Syntax

data Context = Context
    { _types :: Map Text LLVM.AST.Type
    , _values :: Map Text LLVM.AST.Operand
    , _symbolCounter :: Int
    }

types :: Lens' Context (Map Text LLVM.AST.Type)
types = lens _types (\s ts -> s {_types = ts})

values :: Lens' Context (Map Text LLVM.AST.Operand)
values = lens _values (\s vs -> s {_values = vs})

symbolCounter :: Lens' Context Int
symbolCounter = lens _symbolCounter (\s i -> s {_symbolCounter = i})

type ModuleBuilder = LLVM.IRBuilder.ModuleBuilderT (State Context)

type IRBuilder = LLVM.IRBuilder.IRBuilderT ModuleBuilder

buildModule :: ShortByteString -> ModuleBuilder a -> LLVM.AST.Module
buildModule n builder =
    evalState (LLVM.IRBuilder.buildModuleT n builder) (Context mempty mempty 0)

-- These helper functions are polymorphic in the monad @m@ so that I can use
-- them with both @ModuleBuilder@ and @IRBuilder@.
bindType :: MonadState Context m => Text -> LLVM.AST.Type -> m ()
bindType n t = types %= Map.insert n t

findType :: MonadState Context m => Text -> m LLVM.AST.Type
findType n = fmap (Map.! n) (use types)

bindValue :: MonadState Context m => Text -> LLVM.AST.Operand -> m ()
bindValue x a = values %= Map.insert x a

findValue :: MonadState Context m => Text -> m LLVM.AST.Operand
findValue x = fmap (Map.! x) (use values)

freshSymbolName :: MonadState Context m => m LLVM.AST.Name
freshSymbolName = do
    i <- symbolCounter <<%= (+ 1)
    pure (reifyName ("symbol." <> Text.pack (show i)))

namespaced :: MonadState Context m => m a -> m a
namespaced p = do
    backup <- use values
    result <- p
    values .= backup
    pure result

structName :: Text -> Text
structName = ("struct." <>)

enumName :: Text -> Maybe Int -> Text
enumName n Nothing = "enum." <> n
enumName n (Just i) = "enum." <> n <> "." <> Text.pack (show i)

reifyName :: Text -> LLVM.AST.Name
reifyName =
      LLVM.AST.Name
    . ByteString.Short.toShort
    . Text.Encoding.encodeUtf8

reifyType :: Type 'Checked -> LLVM.AST.Type
reifyType = \case
    Unit -> LLVM.AST.NamedTypeReference (reifyName "unit")
    Bool -> LLVM.AST.Type.i1
    I64 -> LLVM.AST.Type.i64
    F64 -> LLVM.AST.Type.double
    Struct n -> LLVM.AST.NamedTypeReference (reifyName (structName n))
    Enum n -> LLVM.AST.NamedTypeReference (reifyName (enumName n Nothing))

defineType :: Text -> LLVM.AST.Type -> ModuleBuilder ()
defineType n t' = do
    td' <- LLVM.IRBuilder.typedef (reifyName n) (Just t')
    bindType n td'

varargBuiltins :: [(Text, [LLVM.AST.Type], LLVM.AST.Type)]
varargBuiltins =
    [("printf", [LLVM.AST.Type.ptr LLVM.AST.Type.i8], LLVM.AST.Type.i32)]

tagType :: LLVM.AST.Type
tagType = LLVM.AST.Type.i8

tagSize :: Int
tagSize = 1

alignment :: Type 'Checked -> Int
alignment = \case
    Unit -> 1
    Bool -> 1
    I64 -> 8
    F64 -> 8
    Struct _ -> 8
    Enum _ -> 8

alignedTo :: Int -> Int -> Int
alignedTo x a = (x + a - 1) `div` a * a

codegen :: Module 'Checked -> ModuleBuilder ()
codegen (Module typeDefs fnDefs) = mdo
    -- Define the unit type.
    defineType "unit" (LLVM.AST.StructureType False mempty)

    for_ typeDefs (\case
        StructDef n0 ts -> do
            let n = structName n0

            let t' = LLVM.AST.StructureType False (fmap reifyType (toList ts))

            defineType n t'
        EnumDef n0 vs -> do
            -- Define the base type.
            let n = enumName n0 Nothing

            let placeholder =
                    LLVM.AST.ArrayType
                        (fromIntegral (size (Enum n0) - tagSize))
                        LLVM.AST.Type.i8

            let t' = LLVM.AST.StructureType False [tagType, placeholder]

            defineType n t'

            -- Recurse into the variants.
            ifor_ vs (\i (EnumVariant _ ts) -> do
                let vn = enumName n0 (Just i)

                let vt' =
                        LLVM.AST.StructureType
                            False
                            (tagType : fmap reifyType (toList ts))

                defineType vn vt'))

    -- Declare variadic built-in functions like printf.
    for_ varargBuiltins (\(n, ats, returnType) -> do
        reference <- LLVM.IRBuilder.externVarArgs (reifyName n) ats returnType
        bindValue n reference)

    -- Add all functions to the 'Context'.
    zipWithM_ (\(FnDef (FnDecl n _ _) _) fn -> bindValue n fn) fnDefs fns

    -- Define the functions.
    fns <- traverse fnDef fnDefs

    pure ()
  where
    structFields = Map.fromList [(n, ts) | StructDef n ts <- typeDefs]

    enumVariants =
        Map.fromList
            [(n, [ts | EnumVariant _ ts <- vs]) | EnumDef n vs <- typeDefs]

    sizeMemo = Map.fromList (
            [(Unit, 0), (Bool, 1), (I64, 8), (F64, 8)]
        <>  [(t, sizeRec t) | StructDef n _ <- typeDefs, let t = Struct n]
        <>  [(t, sizeRec t) | EnumDef n _ <- typeDefs, let t = Enum n])

    sizeRec = \case
        Struct n -> totalSize 0 (structFields Map.! n)
        Enum n -> maximum [totalSize tagSize ts | ts <- enumVariants Map.! n]
        t -> sizeMemo Map.! t
      where
        totalSize =
            foldl' (\x t -> x `alignedTo` alignment t + sizeMemo Map.! t)

    size = (sizeMemo Map.!)

fnDef :: FnDef 'Checked -> ModuleBuilder LLVM.AST.Operand
fnDef (FnDef (FnDecl n as t) b) =
    LLVM.IRBuilder.function n' as' t' body
  where
    n' = reifyName n

    reifyArgument (FnArg _ at) = (reifyType at, LLVM.IRBuilder.NoParameterName)

    as' = [reifyArgument a | a <- toList as]

    t' = reifyType t

    body operands = do
        LLVM.IRBuilder.emitBlockStart (LLVM.AST.Name "entry")
        result <- namespaced (do
            zipWithM_ bindValue [an | FnArg an _ <- toList as] operands
            block b)
        LLVM.IRBuilder.ret result

cast :: Text -> Int -> LLVM.AST.Operand -> IRBuilder LLVM.AST.Operand
cast n i a = do
    t <- findType (enumName n Nothing)
    p <- LLVM.IRBuilder.alloca t Nothing 0
    LLVM.IRBuilder.store p 0 a
    t' <- findType (enumName n (Just i))
    p' <- LLVM.IRBuilder.bitcast p (LLVM.AST.Type.ptr t')
    LLVM.IRBuilder.load p' 0

construct
    :: LLVM.AST.Type
    -> Vector (ExprWithoutBlock 'Checked)
    -> IRBuilder LLVM.AST.Operand
construct t es = do
    p <- LLVM.IRBuilder.alloca t Nothing 0
    Vector.iforM_ es (\i e -> do
        q <- LLVM.IRBuilder.gep p [index 0, index (fromIntegral i)]
        a <- exprWithoutBlock e
        LLVM.IRBuilder.store q 0 a)
    LLVM.IRBuilder.load p 0
  where
    index = LLVM.IRBuilder.int32

destructureStruct :: Vector Text -> LLVM.AST.Operand -> IRBuilder ()
destructureStruct xs a =
    Vector.iforM_ xs (\i x -> do
        b <- LLVM.IRBuilder.extractValue a [fromIntegral i]
        bindValue x b)

destructureEnum :: Vector Text -> LLVM.AST.Operand -> IRBuilder ()
destructureEnum xs a =
    Vector.iforM_ xs (\i x -> do
        -- Add 1 in order to skip the tag.
        b <- LLVM.IRBuilder.extractValue a [fromIntegral i + 1]
        bindValue x b)

block :: Block 'Checked -> IRBuilder LLVM.AST.Operand
block (Block ss e) = do
    traverse_ statement ss
    expr e

statement :: Statement 'Checked -> IRBuilder ()
statement = \case
    LetStatement p e -> do
        a <- expr e
        case p of
            VarPattern x -> bindValue x a
            StructPattern _ xs -> destructureStruct xs a
    ExprStatement e -> void (expr e)

expr :: Expr 'Checked -> IRBuilder LLVM.AST.Operand
expr = \case
    ExprWithBlock e -> exprWithBlock e
    ExprWithoutBlock e -> exprWithoutBlock e

exprWithBlock :: ExprWithBlock 'Checked -> IRBuilder LLVM.AST.Operand
exprWithBlock = \case
    BlockExpr b -> namespaced (block b)
    IfExpr x thenBlock elseBlock -> mdo
        a <- findValue x
        LLVM.IRBuilder.condBr a thenLabel elseLabel
        thenLabel <- LLVM.IRBuilder.block
        thenValue <- namespaced (block thenBlock)
        LLVM.IRBuilder.br joinLabel
        elseLabel <- LLVM.IRBuilder.block
        elseValue <- namespaced (block elseBlock)
        LLVM.IRBuilder.br joinLabel
        joinLabel <- LLVM.IRBuilder.block
        LLVM.IRBuilder.phi [(thenValue, thenLabel), (elseValue, elseLabel)]
    CheckedMatchExpr x n as -> mdo
        a <- findValue x
        tagValue <- LLVM.IRBuilder.extractValue a [0]
        let outgoing = [(tagLit i, vl) | (i, _, vl) <- incoming]
        LLVM.IRBuilder.switch tagValue defaultLabel outgoing
        incoming <- ifor as (\i (CheckedMatchArm xs e) -> do
            vl <- LLVM.IRBuilder.block
            result <- namespaced (do
                a' <- cast n i a
                destructureEnum xs a'
                expr e)
            LLVM.IRBuilder.br joinLabel
            pure (i, result, vl))
        defaultLabel <- LLVM.IRBuilder.block
        -- The default block is unreachable because the match expression covers
        -- all variants.
        LLVM.IRBuilder.unreachable
        joinLabel <- LLVM.IRBuilder.block
        LLVM.IRBuilder.phi [(result, vl) | (_, result, vl) <- incoming]
      where
        tagLit = LLVM.AST.Constant.Int 8 . fromIntegral

exprWithoutBlock :: ExprWithoutBlock 'Checked -> IRBuilder LLVM.AST.Operand
exprWithoutBlock = \case
    LitExpr l -> pure (
        case l of
            UnitLit () -> unitLit
            BoolLit b -> LLVM.IRBuilder.bit (if b then 1 else 0)
            I64Lit x -> LLVM.IRBuilder.int64 x
            F64Lit x -> LLVM.IRBuilder.double x)
    VarExpr x -> findValue x
    UnaryOpExpr o e -> do
        a <- exprWithoutBlock e
        let instruction =
                case o of
                    Neg -> LLVM.IRBuilder.sub (LLVM.IRBuilder.int64 0)
                    FNeg -> LLVM.IRBuilder.fsub (LLVM.IRBuilder.double 0)
                    Not -> LLVM.IRBuilder.xor (LLVM.IRBuilder.bit 1)
                    AsI64 -> flip LLVM.IRBuilder.fptosi (reifyType I64)
        instruction a
    BinaryOpExpr o e0 e1 -> do
        a0 <- exprWithoutBlock e0
        a1 <- exprWithoutBlock e1
        let instruction =
                case o of
                    Mul -> LLVM.IRBuilder.mul
                    FMul -> LLVM.IRBuilder.fmul
                    Div -> LLVM.IRBuilder.sdiv
                    FDiv -> LLVM.IRBuilder.fdiv
                    Rem -> LLVM.IRBuilder.srem
                    FRem -> LLVM.IRBuilder.frem
                    Add -> LLVM.IRBuilder.add
                    FAdd -> LLVM.IRBuilder.fadd
                    Sub -> LLVM.IRBuilder.sub
                    FSub -> LLVM.IRBuilder.fsub
                    Shl -> LLVM.IRBuilder.shl
                    Shr -> LLVM.IRBuilder.ashr
                    BitAnd -> LLVM.IRBuilder.and
                    BitXor -> LLVM.IRBuilder.xor
                    BitOr -> LLVM.IRBuilder.or
                    Icmp p ->
                        LLVM.IRBuilder.icmp
                            (case p of
                                Eq -> LLVM.AST.IntegerPredicate.EQ
                                Ne -> LLVM.AST.IntegerPredicate.NE
                                Lt -> LLVM.AST.IntegerPredicate.SLT
                                Gt -> LLVM.AST.IntegerPredicate.SGT
                                Le -> LLVM.AST.IntegerPredicate.SLE
                                Ge -> LLVM.AST.IntegerPredicate.SGE)
                    Fcmp p ->
                        LLVM.IRBuilder.fcmp
                            (case p of
                                Eq -> LLVM.AST.FloatingPointPredicate.OEQ
                                Ne -> LLVM.AST.FloatingPointPredicate.ONE
                                Lt -> LLVM.AST.FloatingPointPredicate.OLT
                                Gt -> LLVM.AST.FloatingPointPredicate.OGT
                                Le -> LLVM.AST.FloatingPointPredicate.OLE
                                Ge -> LLVM.AST.FloatingPointPredicate.OGE)
                    And -> LLVM.IRBuilder.and
                    Or -> LLVM.IRBuilder.or
        instruction a0 a1
    CallExpr c -> call c
    StructExpr n es -> do
        t <- findType (structName n)
        construct t es
    EnumExpr n (EnumVariantIndex i) es -> do
        t <- findType (enumName n (Just i))
        construct t es
    PrintLnExpr (CheckedFormatString f) es -> do
        fn <- findValue "printf"
        symbolName <- freshSymbolName
        symbol <- LLVM.IRBuilder.globalStringPtr (Text.unpack f) symbolName
        let a0 = LLVM.AST.ConstantOperand symbol
        as <- traverse exprWithoutBlock es
        LLVM.IRBuilder.call fn [(a, mempty) | a <- a0 : as]
  where
    unitLit =
        LLVM.AST.ConstantOperand
            (LLVM.AST.Constant.Struct (Just (reifyName "unit")) False mempty)

call :: Call 'Checked -> IRBuilder LLVM.AST.Operand
call (Call n es) = do
    fn <- findValue n
    as <- traverse exprWithoutBlock es
    LLVM.IRBuilder.call fn [(a, mempty) | a <- toList as]