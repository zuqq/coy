{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Coy.Codegen (buildModule, codegen) where

import Control.Monad (void, zipWithM_)
-- We need to use lazy @State@ for @-XRecursiveDo@.
import Control.Monad.Trans.State (State, evalState)
import Data.Foldable (foldl', for_, toList, traverse_)
import Data.List.Index (ifor, ifor_)
import Data.Map (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Lens.Micro (Lens', lens)
import Lens.Micro.Mtl ((%=), (.=), (<<%=), use)

import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Short as ByteString.Short
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Vector as Vector
import qualified LLVM.AST
import qualified LLVM.AST.Constant
import qualified LLVM.AST.FloatingPointPredicate
import qualified LLVM.AST.Global
import qualified LLVM.AST.IntegerPredicate
import qualified LLVM.AST.Linkage
import qualified LLVM.AST.Type
import qualified LLVM.IRBuilder.Extended as LLVM.IRBuilder

import Coy.Syntax

data Context = Context
    { _fns :: Map Text LLVM.AST.Operand
    , _values :: Map Text LLVM.AST.Operand
    , _symbolCounter :: Int
    }

fns :: Lens' Context (Map Text LLVM.AST.Operand)
fns = lens _fns (\s fs -> s {_fns = fs})

values :: Lens' Context (Map Text LLVM.AST.Operand)
values = lens _values (\s vs -> s {_values = vs})

symbolCounter :: Lens' Context Int
symbolCounter = lens _symbolCounter (\s i -> s {_symbolCounter = i})

type ModuleBuilder = LLVM.IRBuilder.ModuleBuilderT (State Context)

type IRBuilder = LLVM.IRBuilder.IRBuilderT ModuleBuilder

buildModule :: String -> ModuleBuilder a -> LLVM.AST.Module
buildModule n builder =
    evalState (LLVM.IRBuilder.buildModuleT n' builder) (Context mempty mempty 0)
  where
    n' = ByteString.Short.toShort (ByteString.Char8.pack n)

bindFn :: Text -> LLVM.AST.Operand -> ModuleBuilder ()
bindFn n t = fns %= Map.insert n t

findFn :: Text -> IRBuilder LLVM.AST.Operand
findFn n = fmap (Map.! n) (use fns)

bindValue :: Text -> LLVM.AST.Operand -> IRBuilder ()
bindValue x a = values %= Map.insert x a

findValue :: Text -> IRBuilder LLVM.AST.Operand
findValue x = fmap (Map.! x) (use values)

namespaced :: IRBuilder a -> IRBuilder a
namespaced p = do
    backup <- use values
    result <- p
    values .= backup
    pure result

structName :: Text -> Text
structName = ("struct." <>)

enumName :: Text -> Maybe Int -> Text
enumName n = \case
    Nothing -> "enum." <> n
    Just i -> "enum." <> n <> "." <> Text.pack (show i)

fnName :: Text -> Text
fnName = ("fn." <>)

symbolName :: Int -> Text
symbolName i = "symbol." <> Text.pack (show i)

reifyName :: Text -> LLVM.AST.Name
reifyName =
      LLVM.AST.Name
    . ByteString.Short.toShort
    . Text.Encoding.encodeUtf8

reifyStruct :: Text -> LLVM.AST.Type
reifyStruct = LLVM.AST.NamedTypeReference . reifyName . structName

reifyEnum :: Text -> Maybe Int -> LLVM.AST.Type
reifyEnum n = LLVM.AST.NamedTypeReference . reifyName . enumName n

reifyType :: Type 'Checked -> LLVM.AST.Type
reifyType = \case
    Unit -> LLVM.AST.NamedTypeReference "unit"
    Bool -> LLVM.AST.Type.i1
    I64 -> LLVM.AST.Type.i64
    F64 -> LLVM.AST.Type.double
    Struct n -> reifyStruct n
    Enum n -> reifyEnum n Nothing

-- Whether or not values of this type are operated on through pointers.
hasPointerOperandType :: Type 'Checked -> Bool
hasPointerOperandType = \case
    Unit -> False
    Bool -> False
    I64 -> False
    F64 -> False
    Struct _ -> True
    Enum _ -> True

operandType :: Type 'Checked -> LLVM.AST.Type
operandType t
    | hasPointerOperandType t = LLVM.AST.Type.ptr (reifyType t)
    | otherwise = reifyType t

freshSymbolName :: IRBuilder LLVM.AST.Name
freshSymbolName = do
    i <- symbolCounter <<%= (+ 1)
    pure (reifyName (symbolName i))

globalReference
    :: LLVM.AST.Type
    -- ^ Type of the global variable.
    -> LLVM.AST.Name
    -- ^ Name of the global variable.
    -> LLVM.AST.Operand
globalReference t' =
    LLVM.AST.ConstantOperand . LLVM.AST.Constant.GlobalReference t'

localReference
    :: LLVM.AST.Type
    -- ^ Type of the local variable.
    -> Word
    -- ^ Number of the local variable.
    -> LLVM.AST.Operand
localReference t' = LLVM.AST.LocalReference t' . LLVM.AST.UnName

intrinsicFns :: [(Text, LLVM.AST.Name, [LLVM.AST.Type], LLVM.AST.Type)]
intrinsicFns =
    [ ("cos", "llvm.cos.f64", [LLVM.AST.Type.double], LLVM.AST.Type.double)
    , ("sin", "llvm.sin.f64", [LLVM.AST.Type.double], LLVM.AST.Type.double)
    , ("sqrt", "llvm.sqrt.f64", [LLVM.AST.Type.double], LLVM.AST.Type.double)
    ]

memcpyName :: LLVM.AST.Name
memcpyName = "llvm.memcpy.p0i8.p0i8.i32"

memcpyArgTypes :: [LLVM.AST.Type]
memcpyArgTypes =
    [ LLVM.AST.Type.ptr LLVM.AST.Type.i8
    , LLVM.AST.Type.ptr LLVM.AST.Type.i8
    , LLVM.AST.Type.i32
    , LLVM.AST.Type.i1
    ]

memcpyReturnType :: LLVM.AST.Type
memcpyReturnType = LLVM.AST.Type.void

memcpy :: LLVM.AST.Operand
memcpy = globalReference memcpyFnType memcpyName
  where
    memcpyFnType =
        LLVM.AST.Type.ptr
            (LLVM.AST.FunctionType memcpyReturnType memcpyArgTypes False)

printfName :: LLVM.AST.Name
printfName = "printf"

printfArgTypes :: [LLVM.AST.Type]
printfArgTypes = [LLVM.AST.Type.ptr LLVM.AST.Type.i8]

printfReturnType :: LLVM.AST.Type
printfReturnType = LLVM.AST.Type.i32

printf :: LLVM.AST.Operand
printf = globalReference printfFnType printfName
  where
    printfFnType =
        LLVM.AST.Type.ptr
            (LLVM.AST.FunctionType printfReturnType printfArgTypes True)

index :: Integer -> LLVM.AST.Operand
index = LLVM.IRBuilder.int32

tagType :: LLVM.AST.Type
tagType = LLVM.AST.Type.i64

tagLit :: Int -> LLVM.AST.Constant.Constant
tagLit = LLVM.AST.Constant.Int 64 . fromIntegral

-- Size of the tag in bytes.
tagSize :: Int
tagSize = 8

-- Alignment of a type in bytes, as specified by the default data layout.
alignment :: Type 'Checked -> Int
alignment = \case
    Unit -> 8
    Bool -> 1
    I64 -> 8
    F64 -> 8
    Struct _ -> 8
    Enum _ -> 8

alignedTo :: Int -> Int -> Int
alignedTo x a = (x + a - 1) `div` a * a

computeEnumSizes :: [TypeDef 'Checked] -> Map Text Int
computeEnumSizes typeDefs = enumSizes
  where
    structSizes = Map.fromList [(n, structSize ts) | StructDef n ts <- typeDefs]

    enumSizes = Map.fromList [(n, enumSize vs) | EnumDef n vs <- typeDefs]

    structSize = totalSize 0

    enumSize vs = maximum [enumVariantSize ts | EnumVariant _ ts <- vs]

    enumVariantSize = totalSize tagSize

    totalSize = foldl' (\x t -> x `alignedTo` alignment t + fieldSize t)

    fieldSize = \case
        Unit -> 0
        Bool -> 1
        I64 -> 8
        F64 -> 8
        Struct n -> structSizes Map.! n
        Enum n -> enumSizes Map.! n

codegen :: Module 'Checked -> ModuleBuilder ()
-- Here and elsewhere the @-XRecursiveDo@ extension allows me to use forward
-- references without too much trouble.
codegen (CheckedModule typeDefs (FnDef _ mainBlock) otherFnDefs) = mdo
    -- Define the unit type.
    void (defineType "unit" (LLVM.AST.StructureType False mempty))

    for_ typeDefs (\case
        StructDef n0 ts -> structDef n0 ts
        EnumDef n0 vs -> enumDef n0 vs)

    -- Declare intrinsic functions.
    for_ intrinsicFns (\(n, n', ats', t') -> do
        reference <- LLVM.IRBuilder.extern n' ats' t'
        bindFn n reference)

    -- Declare @memcpy@.
    void (LLVM.IRBuilder.extern memcpyName memcpyArgTypes memcpyReturnType)

    -- Declare @printf@.
    void (
        LLVM.IRBuilder.externVarArgs printfName printfArgTypes printfReturnType)

    -- Add non-main functions to the 'Context'.
    zipWithM_ (\fd fn -> bindFn (fnDefName fd) fn) otherFnDefs otherFns

    otherFns <- traverse fnDef otherFnDefs

    -- Define the main function.
    let mainArgs = mempty

    let mainReturnType = reifyType Unit

    let mainBody _ = do
            LLVM.IRBuilder.emitBlockStart "entry"
            namespaced (tailBlock mainBlock)

    void (LLVM.IRBuilder.function "main" mainArgs mainReturnType mainBody)
  where
    defineType n t' = LLVM.IRBuilder.typedef (reifyName n) (Just t')

    structDef n0 ts = do
        let n = structName n0

        let t' = LLVM.AST.StructureType False (fmap reifyType (toList ts))

        void (defineType n t')

    enumSizes = computeEnumSizes typeDefs

    enumDef n0 vs = do
        -- Define the base type.
        let n = enumName n0 Nothing

        let k = fromIntegral (enumSizes Map.! n0 - tagSize)

        let placeholder = LLVM.AST.ArrayType k LLVM.AST.Type.i8

        let t' = LLVM.AST.StructureType False [tagType, placeholder]

        void (defineType n t')

        -- Recurse into the variants.
        ifor_ vs (\i (EnumVariant _ ts) -> do
            let vn = enumName n0 (Just i)

            let ts' = fmap reifyType (toList ts)

            let vt' = LLVM.AST.StructureType False (tagType : ts')

            defineType vn vt')

fnDef :: FnDef 'Checked -> ModuleBuilder LLVM.AST.Operand
fnDef (FnDef (FnDecl n as t) b) = do
    let n' = reifyName (fnName n)

    let defineFn = LLVM.IRBuilder.functionWith privateLinkage n'

    let ats' = [operandType at | FnArg _ at <- toList as]

    let ans = [an | FnArg an _ <- toList as]

    if hasPointerOperandType t then do
        let returnArgType = operandType t

        let body operands = do
                LLVM.IRBuilder.emitBlockStart "entry"
                namespaced (do
                    zipWithM_ bindValue ans (tail operands)
                    tailBlock b)

        defineFn (returnArgType : ats') LLVM.AST.Type.void body
    else do
        let t' = reifyType t

        let body operands = do
                LLVM.IRBuilder.emitBlockStart "entry"
                namespaced (do
                    zipWithM_ bindValue ans operands
                    tailBlock b)

        defineFn ats' t' body
  where
    -- Define non-main functions with private linkage in order to unlock
    -- further optimizations.
    privateLinkage = LLVM.AST.functionDefaults
            {LLVM.AST.Global.linkage = LLVM.AST.Linkage.Private}

copyTo
    :: LLVM.AST.Operand
    -- ^ Destination pointer.
    -> LLVM.AST.Operand
    -- ^ Source pointer.
    -> LLVM.AST.Type
    -- ^ Referent type.
    -> IRBuilder ()
copyTo dest0 src0 t' = do
    dest <- castToVoidPointer dest0

    src <- castToVoidPointer src0

    let len = LLVM.AST.ConstantOperand (LLVM.AST.Constant.sizeof t')

    let isvolatile = LLVM.IRBuilder.bit 0

    let as' =
            [(dest, mempty), (src, mempty), (len, mempty), (isvolatile, mempty)]

    void (LLVM.IRBuilder.call memcpy as')
  where
    castToVoidPointer p =
        LLVM.IRBuilder.bitcast p (LLVM.AST.Type.ptr LLVM.AST.Type.i8)

copyToReturnArg
    :: LLVM.AST.Operand
    -- ^ Source pointer.
    -> LLVM.AST.Type
    -- ^ Referent type.
    -> IRBuilder ()
copyToReturnArg src0 t' = copyTo returnArg src0 t'
  where
    returnArg = localReference (LLVM.AST.Type.ptr t') 0

constructStruct
    :: Text
    -- ^ Name of the struct.
    -> Vector (ExprWithoutBlock 'Checked, Type 'Checked)
    -- ^ Values for the components with their types.
    -> IRBuilder LLVM.AST.Operand
constructStruct n ets = do
    p <- LLVM.IRBuilder.alloca (reifyStruct n) Nothing 0
    Vector.iforM_ ets (\i (e, at) -> do
        q <- LLVM.IRBuilder.gep p [index 0, index (fromIntegral i)]
        a <- exprWithoutBlock e
        if hasPointerOperandType at then do
            copyTo q a (reifyType at)
        else
            LLVM.IRBuilder.store q 0 a)
    pure p

constructEnumVariant
    :: Text
    -- ^ Name of the enum.
    -> Int
    -- ^ Index of the enum variant.
    -> Vector (ExprWithoutBlock 'Checked, Type 'Checked)
    -- ^ Values for the components with their types.
    -> IRBuilder LLVM.AST.Operand
constructEnumVariant n i ets = do
    p0 <- LLVM.IRBuilder.alloca (reifyEnum n Nothing) Nothing 0
    tagPointer <- LLVM.IRBuilder.bitcast p0 (LLVM.AST.Type.ptr tagType)
    LLVM.IRBuilder.store tagPointer 0 (LLVM.AST.ConstantOperand (tagLit i))
    p <- LLVM.IRBuilder.bitcast p0 (LLVM.AST.Type.ptr (reifyEnum n (Just i)))
    Vector.iforM_ ets (\k (e, at) -> do
        q <- LLVM.IRBuilder.gep p [index 0, index (fromIntegral k + 1)]
        a <- exprWithoutBlock e
        if hasPointerOperandType at then do
            copyTo q a (reifyType at)
        else
            LLVM.IRBuilder.store q 0 a)
    pure p0

destructureStruct
    :: Vector (Text, Type 'Checked)
    -- ^ Variables for the components with their types.
    -> LLVM.AST.Operand
    -- ^ Pointer to the value.
    -> IRBuilder ()
destructureStruct xts p =
    Vector.iforM_ xts (\k (x, at) -> do
        q <- LLVM.IRBuilder.gep p [index 0, index (fromIntegral k)]
        if hasPointerOperandType at then
            bindValue x q
        else do
            b <- LLVM.IRBuilder.load q 0
            bindValue x b)

destructureEnumVariant
    :: Text
    -- ^ Name of the enum.
    -> Int
    -- ^ Index of the enum variant.
    -> Vector (Text, Type 'Checked)
    -- ^ Variables for the components with their types.
    -> LLVM.AST.Operand
    -- ^ Pointer to the value.
    -> IRBuilder ()
destructureEnumVariant n i xts p0 = do
    p <- LLVM.IRBuilder.bitcast p0 (LLVM.AST.Type.ptr (reifyEnum n (Just i)))
    Vector.iforM_ xts (\k (x, at) -> do
        q <- LLVM.IRBuilder.gep p [index 0, index (fromIntegral k + 1)]
        if hasPointerOperandType at then
            bindValue x q
        else do
            b <- LLVM.IRBuilder.load q 0
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
            CheckedStructPattern xts -> destructureStruct xts a
    ExprStatement e -> void (expr e)

expr :: Expr 'Checked -> IRBuilder LLVM.AST.Operand
expr = \case
    ExprWithBlock e -> exprWithBlock e
    ExprWithoutBlock e -> exprWithoutBlock e

exprWithBlock :: ExprWithBlock 'Checked -> IRBuilder LLVM.AST.Operand
exprWithBlock = \case
    BlockExpr b -> namespaced (block b)
    IfExpr e thenBlock elseBlock -> mdo
        a <- exprWithoutBlock e
        LLVM.IRBuilder.condBr a thenInLabel elseInLabel

        thenInLabel <- LLVM.IRBuilder.block
        thenOut <- namespaced (do
            thenValue <- block thenBlock
            thenOutLabel <- LLVM.IRBuilder.currentBlock
            pure (thenValue, thenOutLabel))
        LLVM.IRBuilder.br joinLabel

        elseInLabel <- LLVM.IRBuilder.block
        elseOut <- namespaced (do
            elseValue <- block elseBlock
            elseOutLabel <- LLVM.IRBuilder.currentBlock
            pure (elseValue, elseOutLabel))
        LLVM.IRBuilder.br joinLabel

        joinLabel <- LLVM.IRBuilder.block
        LLVM.IRBuilder.phi [thenOut, elseOut]
    CheckedMatchExpr e0 n as -> mdo
        p0 <- exprWithoutBlock e0
        tagPointer <- LLVM.IRBuilder.bitcast p0 (LLVM.AST.Type.ptr tagType)
        tagValue <- LLVM.IRBuilder.load tagPointer 0
        let outgoing = [(tagLit i, inLabel) | ((i, inLabel), _) <- branches]
        LLVM.IRBuilder.switch tagValue defaultLabel outgoing

        branches <- ifor as (\i (CheckedMatchArm xts e) -> do
            inLabel <- LLVM.IRBuilder.block
            result <- namespaced (do
                destructureEnumVariant n i xts p0
                expr e)
            outLabel <- LLVM.IRBuilder.currentBlock
            LLVM.IRBuilder.br joinLabel
            pure ((i, inLabel), (result, outLabel)))

        defaultLabel <- LLVM.IRBuilder.block
        -- The default block is unreachable because the match expression covers
        -- all variants.
        LLVM.IRBuilder.unreachable

        joinLabel <- LLVM.IRBuilder.block
        LLVM.IRBuilder.phi (fmap snd branches)

exprWithoutBlock :: ExprWithoutBlock 'Checked -> IRBuilder LLVM.AST.Operand
exprWithoutBlock = \case
    LitExpr l -> pure (
        case l of
            UnitLit () -> unitLit
            BoolLit b -> LLVM.IRBuilder.bit (if b then 1 else 0)
            I64Lit x -> LLVM.IRBuilder.int64 x
            F64Lit x -> LLVM.IRBuilder.double x)
    CheckedVarExpr x _ -> findValue x
    UnaryOpExpr o e -> do
        a <- exprWithoutBlock e
        let instruction =
                case o of
                    Neg -> LLVM.IRBuilder.sub (LLVM.IRBuilder.int64 0)
                    FNeg -> LLVM.IRBuilder.fsub (LLVM.IRBuilder.double 0)
                    Not -> LLVM.IRBuilder.xor (LLVM.IRBuilder.bit 1)
                    AsF64 -> flip LLVM.IRBuilder.sitofp LLVM.AST.Type.double
                    AsI64 -> flip LLVM.IRBuilder.fptosi LLVM.AST.Type.i64
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
    CheckedCallExpr n es returnType -> do
        fn <- findFn n
        as <- traverse exprWithoutBlock es
        let as' = [(a, mempty) | a <- toList as]
        if hasPointerOperandType returnType then do
            returnArg <- LLVM.IRBuilder.alloca (reifyType returnType) Nothing 0
            void (LLVM.IRBuilder.call fn ((returnArg, mempty) : as'))
            pure returnArg
        else
            LLVM.IRBuilder.call fn as'
    CheckedStructExpr n ets -> constructStruct n ets
    CheckedEnumExpr n i ets -> constructEnumVariant n i ets
    PrintLnExpr (CheckedFormatString f) es -> do
        n <- freshSymbolName
        s <- LLVM.IRBuilder.globalStringPtr (Text.unpack f) n
        let a0 = LLVM.AST.ConstantOperand s
        as <- traverse exprWithoutBlock es
        void (LLVM.IRBuilder.call printf [(a, mempty) | a <- a0 : as])
        pure unitLit
  where
    unitLit =
        LLVM.AST.ConstantOperand
            (LLVM.AST.Constant.Struct (Just "unit") False mempty)

-- Entry point for blocks in tail position.
tailBlock :: Block 'Checked -> IRBuilder ()
tailBlock (Block ss e) = do
    traverse_ statement ss
    tailExpr e

tailExpr :: Expr 'Checked -> IRBuilder ()
tailExpr = \case
    ExprWithBlock e -> tailExprWithBlock e
    ExprWithoutBlock e -> tailExprWithoutBlock e

-- Instead of joining the branches by introducing a phi node and then returning,
-- we return separately in each branch. This has the effect that function calls
-- that are in tail position in the AST are also in tail position in the
-- generated LLVM IR.
tailExprWithBlock :: ExprWithBlock 'Checked -> IRBuilder ()
tailExprWithBlock = \case
    BlockExpr b -> namespaced (tailBlock b)
    IfExpr e thenBlock elseBlock -> mdo
        a <- exprWithoutBlock e
        LLVM.IRBuilder.condBr a thenLabel elseLabel

        thenLabel <- LLVM.IRBuilder.block
        namespaced (tailBlock thenBlock)

        elseLabel <- LLVM.IRBuilder.block
        namespaced (tailBlock elseBlock)
    CheckedMatchExpr e0 n as -> mdo
        p0 <- exprWithoutBlock e0
        tagPointer <- LLVM.IRBuilder.bitcast p0 (LLVM.AST.Type.ptr tagType)
        tagValue <- LLVM.IRBuilder.load tagPointer 0
        LLVM.IRBuilder.switch tagValue defaultLabel outgoing

        outgoing <- ifor as (\i (CheckedMatchArm xts e) -> do
            label <- LLVM.IRBuilder.block
            namespaced (do
                destructureEnumVariant n i xts p0
                tailExpr e)
            pure (tagLit i, label))

        defaultLabel <- LLVM.IRBuilder.block
        LLVM.IRBuilder.unreachable

tailExprWithoutBlock :: ExprWithoutBlock 'Checked -> IRBuilder ()
tailExprWithoutBlock = \case
    e@(CheckedVarExpr _ t) -> do
        result <- exprWithoutBlock e
        if hasPointerOperandType t then do
            copyToReturnArg result (reifyType t)
            LLVM.IRBuilder.retVoid
        else
            LLVM.IRBuilder.ret result
    CheckedCallExpr n es returnType -> do
        fn <- findFn n
        as <- traverse exprWithoutBlock es
        let as' = [(a, mempty) | a <- toList as]
        if hasPointerOperandType returnType then do
            -- We know that the return type of the callee is equal to the return
            -- type of the caller, so we can reuse the caller's return argument.
            let returnArg = localReference (operandType returnType) 0
            void (LLVM.IRBuilder.call fn ((returnArg, mempty) : as'))
            LLVM.IRBuilder.retVoid
        else do
            result <- LLVM.IRBuilder.call fn as'
            LLVM.IRBuilder.ret result
    e@(CheckedStructExpr n _) -> do
        result <- exprWithoutBlock e
        copyToReturnArg result (reifyStruct n)
        LLVM.IRBuilder.retVoid
    e@(CheckedEnumExpr n _ _) -> do
        result <- exprWithoutBlock e
        copyToReturnArg result (reifyEnum n Nothing)
        LLVM.IRBuilder.retVoid
    e -> do
        result <- exprWithoutBlock e
        LLVM.IRBuilder.ret result
