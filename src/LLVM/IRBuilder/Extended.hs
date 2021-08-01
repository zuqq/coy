{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module LLVM.IRBuilder.Extended
    (
    -- * Additions
      functionWith
    , privateConstGlobal
    , privateGlobalStringPtr
    -- * Reexports
    , module LLVM.IRBuilder
    )
    where

import Data.List (genericLength)
import Data.Traversable (for)
import LLVM.AST
import LLVM.AST.Constant
import LLVM.AST.Global
import LLVM.AST.Linkage
import LLVM.AST.Type
import LLVM.IRBuilder

-- | A custom version of 'function' with an additional 'Global' argument for
-- setting function attributes and no support for named arguments.
functionWith
    :: MonadModuleBuilder m
    => Global
    -- ^ Function defaults; see 'functionDefaults'.
    -> Name
    -- ^ Function name.
    -> [Type]
    -- ^ Argument types.
    -> Type
    -- ^ Return type.
    -> ([Operand] -> IRBuilderT m ())
    -- ^ Function body.
    -> m Operand
functionWith functionDefaults' n' ats' t' body = do
    (ans', bs') <- runIRBuilderT emptyIRBuilder (do
        ans' <- for ats' (const fresh)
        body (zipWith LocalReference ats' ans')
        pure ans')

    let fnDef' = GlobalDefinition functionDefaults'
            { name = n'
            , parameters = (zipWith reifyArgument ans' ats', False)
            , returnType = t'
            , basicBlocks = bs'
            }

    emitDefn fnDef'

    let fnType' = ptr (FunctionType t' ats' False)

    pure (ConstantOperand (GlobalReference fnType' n'))
  where
    reifyArgument an' at' = Parameter at' an' mempty

-- | A version of 'global' with private linkage.
privateConstGlobal
    :: MonadModuleBuilder m
    => LLVM.AST.Name
    -> LLVM.AST.Type
    -> LLVM.AST.Constant.Constant
    -> m LLVM.AST.Operand
privateConstGlobal x' t' c' = do
    let globalVariableDef' =
            LLVM.AST.GlobalDefinition LLVM.AST.globalVariableDefaults
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

    let globalVariableDef' =
            LLVM.AST.GlobalDefinition LLVM.AST.globalVariableDefaults
                { LLVM.AST.Global.name = x'
                , LLVM.AST.Global.type' = t'
                , LLVM.AST.Global.linkage = LLVM.AST.Linkage.Private
                , LLVM.AST.Global.isConstant = True
                , LLVM.AST.Global.initializer =
                    Just (LLVM.AST.Constant.Array char payload)
                , LLVM.AST.Global.unnamedAddr = Just LLVM.AST.GlobalAddr
                }

    emitDefn globalVariableDef'

    let reference = LLVM.AST.Constant.GlobalReference (LLVM.AST.Type.ptr t') x'

    pure (LLVM.AST.Constant.GetElementPtr True reference [index 0, index 0])
  where
    charLit = LLVM.AST.Constant.Int 8 . fromIntegral . fromEnum

    index = LLVM.AST.Constant.Int 32
