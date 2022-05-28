{-# LANGUAGE BlockArguments #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module LLVM.IRBuilder.Extended
    (
    -- * Additions
      privateConstGlobal
    , privateFunction
    , privateGlobalStringPtr
    -- * Reexports
    , module LLVM.IRBuilder
    )
    where

import Data.List (genericLength)
import LLVM.AST.ParameterAttribute (ParameterAttribute)
import LLVM.IRBuilder

import qualified LLVM.AST
import qualified LLVM.AST.Constant
import qualified LLVM.AST.Global
import qualified LLVM.AST.Linkage
import qualified LLVM.AST.Type

-- | A version of 'function' with private linkage.
privateFunction
    :: MonadModuleBuilder m
    => LLVM.AST.Name
    -- ^ Function name.
    -> [(LLVM.AST.Type, [ParameterAttribute])]
    -- ^ Argument metadata.
    -> LLVM.AST.Type
    -- ^ Return type.
    -> ([LLVM.AST.Operand] -> IRBuilderT m ())
    -- ^ Function body.
    -> m LLVM.AST.Operand
privateFunction n' metadata t' body = do
    let ats' = fmap fst metadata

    (ans', bs') <- runIRBuilderT emptyIRBuilder do
        ans' <- traverse (const fresh) metadata
        body (zipWith LLVM.AST.LocalReference ats' ans')
        pure ans'

    let fnDef' = LLVM.AST.GlobalDefinition LLVM.AST.functionDefaults
            { LLVM.AST.Global.linkage = LLVM.AST.Linkage.Private
            , LLVM.AST.Global.returnType = t'
            , LLVM.AST.Global.name = n'
            , LLVM.AST.Global.parameters = (zipWith decorateArg ans' metadata, False)
            , LLVM.AST.Global.basicBlocks = bs'
            }

    emitDefn fnDef'

    let fnType' = LLVM.AST.Type.ptr (LLVM.AST.Type.FunctionType t' ats' False)

    let reference = LLVM.AST.Constant.GlobalReference fnType' n'

    pure (LLVM.AST.ConstantOperand reference)
  where
    decorateArg an' (at', as) = LLVM.AST.Parameter at' an' as

-- | A version of 'global' with private linkage.
privateConstGlobal
    :: MonadModuleBuilder m
    => LLVM.AST.Name
    -> LLVM.AST.Type
    -> LLVM.AST.Constant.Constant
    -> m LLVM.AST.Operand
privateConstGlobal x' t' c' = do
    let globalVariableDef' = LLVM.AST.GlobalDefinition LLVM.AST.globalVariableDefaults
            { LLVM.AST.Global.name = x'
            , LLVM.AST.Global.type' = t'
            , LLVM.AST.Global.linkage = LLVM.AST.Linkage.Private
            , LLVM.AST.Global.isConstant = True
            , LLVM.AST.Global.initializer = Just c'
            }

    emitDefn globalVariableDef'

    let reference = LLVM.AST.Constant.GlobalReference (LLVM.AST.Type.ptr t') x'

    pure (LLVM.AST.ConstantOperand reference)

-- | A version of 'globalStringPtr' with private linkage.
privateGlobalStringPtr
    :: MonadModuleBuilder m
    => String
    -> LLVM.AST.Name
    -> m LLVM.AST.Constant.Constant
privateGlobalStringPtr s x' = do
    let char = LLVM.AST.IntegerType 8

    let payload = fmap charLit s <> [charLit '\0']

    let t' = LLVM.AST.ArrayType (genericLength payload) char

    let globalVariableDef' = LLVM.AST.GlobalDefinition LLVM.AST.globalVariableDefaults
            { LLVM.AST.Global.name = x'
            , LLVM.AST.Global.type' = t'
            , LLVM.AST.Global.linkage = LLVM.AST.Linkage.Private
            , LLVM.AST.Global.isConstant = True
            , LLVM.AST.Global.initializer = Just (LLVM.AST.Constant.Array char payload)
            , LLVM.AST.Global.unnamedAddr = Just LLVM.AST.GlobalAddr
            }

    emitDefn globalVariableDef'

    let reference = LLVM.AST.Constant.GlobalReference (LLVM.AST.Type.ptr t') x'

    pure (LLVM.AST.Constant.GetElementPtr True reference [index 0, index 0])
  where
    charLit = LLVM.AST.Constant.Int 8 . fromIntegral . fromEnum

    index = LLVM.AST.Constant.Int 32
