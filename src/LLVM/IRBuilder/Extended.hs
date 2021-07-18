{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module LLVM.IRBuilder.Extended
    (
    -- * Additions
      functionWith
    -- * Reexports
    , module LLVM.IRBuilder
    )
    where

import Data.Traversable (for)
import LLVM.AST
import LLVM.AST.Constant
import LLVM.AST.Global
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
