{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Coy.Semantic where

import Algebra.Graph (stars)
import Algebra.Graph.ToGraph (topSort)
import Control.Monad (void, when, zipWithM)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Trans.Except (Except, runExcept)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, runStateT)
import Data.Bifunctor (bimap, first)
import Data.Foldable (for_, toList, traverse_)
import Data.Function (on)
import Data.List (groupBy, intercalate, partition, sortOn)
import Data.List.Index (indexed)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Vector (Vector)
import Data.Void (Void)
import Lens.Micro (Lens', lens)
import Lens.Micro.Mtl (use, view, (%=), (.=))
import Text.Megaparsec

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Lazy.Builder
import qualified Data.Vector as Vector

import Coy.Syntax

data EnumVariantContext = EnumVariantContext
    { enumVariantIndex :: Int
    , enumVariantFieldTypes :: Vector (Type 'Checked)
    }
    deriving Show

invertEnumVariant :: (Int, EnumVariant 'Checked) -> (Text, EnumVariantContext)
invertEnumVariant (i, EnumVariant v ts) = (v, EnumVariantContext i ts)

data Context = Context
    { _structs :: Map Text (Vector (Type 'Checked))
    , _enums :: Map Text (Map Text EnumVariantContext)
    , _consts :: Map Text (Type 'Checked)
    , _strings :: Map Text Int
    , _fns :: Map Text (Vector (Type 'Checked), Type 'Checked)
    , _values :: Map Text (Type 'Checked)
    }
    deriving Show

structs :: Lens' Context (Map Text (Vector (Type 'Checked)))
structs = lens _structs \c ss -> c {_structs = ss}

enums :: Lens' Context (Map Text (Map Text EnumVariantContext))
enums = lens _enums \c es -> c {_enums = es}

consts :: Lens' Context (Map Text (Type 'Checked))
consts = lens _consts \c cs -> c {_consts = cs}

strings :: Lens' Context (Map Text Int)
strings = lens _strings \c ss -> c {_strings = ss}

fns :: Lens' Context (Map Text (Vector (Type 'Checked), Type 'Checked))
fns = lens _fns \c fs -> c {_fns = fs}

values :: Lens' Context (Map Text (Type 'Checked))
values = lens _values \c vs -> c {_values = vs}

data SemanticError
    = RedefinedType (Located (TypeDef 'Unchecked))
    | RedefinedConst (Located (ConstDef 'Unchecked))
    | RedefinedFn (Located (FnDef 'Unchecked))
    | RedefinedEnumVariant Location Text Text
    | StructOrEnumNotFound (Located Text)
    | TypeCycle (NonEmpty (TypeDef 'Unchecked))
    | StructNotFound (Located Text)
    | EnumNotFound (Located Text)
    | EnumVariantNotFound Location Text Text
    | ConstNotFound (Located Text)
    | FnNotFound (Located Text)
    | ValueNotFound (Located Text)
    | ConstDefTypeMismatch Location (Type 'Checked) (Type 'Checked)
    | NegLitInitTypeMismatch (Located (Type 'Checked))
    | ArgumentTypesMismatch Location (Vector (Type 'Checked)) (Vector (Type 'Checked))
    | MainFnDefMissing
    | MainFnDefArityMismatch Location Int
    | MainFnDefReturnTypeMismatch Location (Type 'Checked)
    | FnDefTypeMismatch Location (Type 'Checked) (Type 'Checked)
    | IfScrutineeTypeMismatch Location (Type 'Checked)
    | IfBlocksTypeMismatch Location (Type 'Checked) (Type 'Checked)
    | MatchScrutineeTypeMismatch Location (Type 'Checked)
    | MatchArmEnumMismatch Location Text Text
    | MatchArmEnumVariantNotFound Location Text Text
    | MatchArmResultTypeMismatch Location (Type 'Checked) (Type 'Checked)
    | MatchArmEnumVariantsDuplicated Location Text (NonEmpty Text)
    | MatchArmEnumVariantsMissing Location Text (NonEmpty Text)
    | StructPatternArityMismatch Location Int Int
    | StructPatternTypeMismatch Location (Type 'Checked) (Type 'Checked)
    | UnaryOpTypeMismatch Location (UnaryOp 'Unchecked) (Type 'Checked)
    | BinaryOpTypeMismatch Location (BinaryOp 'Unchecked) (Type 'Checked) (Type 'Checked)
    | PrintExprExcessHole Location
    | PrintExprExcessArg Location
    | TypeNotPrintable (Located (Type 'Checked))
    | IntrinsicFn (Located Text)
    | ReservedName (Located Text)
    deriving Show

type Semantic = StateT Context (Except SemanticError)

evalSemantic :: Semantic a -> Context -> Either SemanticError a
evalSemantic c = runExcept . evalStateT c

runSemantic :: Semantic a -> Context -> Either SemanticError (a, Context)
runSemantic c = runExcept . runStateT c

orFail :: Maybe a -> SemanticError -> Semantic a
orFail x e = maybe (throwError e) pure x

findStruct :: Located Text -> Semantic (Vector (Type 'Checked))
findStruct n = do
    ss <- use structs
    Map.lookup (unpack n) ss `orFail` StructNotFound n

findEnum :: Located Text -> Semantic (Map Text EnumVariantContext)
findEnum n = do
    es <- use enums
    Map.lookup (unpack n) es `orFail` EnumNotFound n

findEnumVariant :: Located Text -> Located Text -> Semantic EnumVariantContext
findEnumVariant n v = do
    e <- findEnum n
    Map.lookup (unpack v) e `orFail` EnumVariantNotFound (locate v) (unpack n) (unpack v)

findConst :: Located Text -> Semantic (Type 'Checked)
findConst n = do
    cs <- use consts
    Map.lookup (unpack n) cs `orFail` ConstNotFound n

bindString :: Text -> Semantic Int
bindString s = do
    ss <- use strings
    let i = Map.findWithDefault (Map.size ss) s ss
    strings .= Map.insert s i ss
    pure i

findFn :: Located Text -> Semantic (Vector (Type 'Checked), Type 'Checked)
findFn n = do
    fs <- use fns
    Map.lookup (unpack n) fs `orFail` FnNotFound n

findValue :: Located Text -> Semantic (Type 'Checked)
findValue x = do
    vs <- use values
    Map.lookup (unpack x) vs `orFail` ValueNotFound x

bindValue :: Text -> Type 'Checked -> Semantic ()
bindValue x t = values %= Map.insert x t

namespaced :: Semantic a -> Semantic a
namespaced p = do
    backup <- use values
    result <- p
    values .= backup
    pure result

intrinsicFns :: [(Text, (Vector (Type 'Checked), Type 'Checked))]
intrinsicFns =
    [ ("cos", (Vector.singleton F64, F64))
    , ("sin", (Vector.singleton F64, F64))
    , ("sqrt", (Vector.singleton F64, F64))
    ]

intrinsicFnNames :: Set Text
intrinsicFnNames = Set.fromList (fmap fst intrinsicFns)

checkIntrinsicFn :: MonadError SemanticError m => Located Text -> m ()
checkIntrinsicFn n = when (unpack n `Set.member` intrinsicFnNames) (throwError (IntrinsicFn n))

reservedNames :: Set Text
reservedNames =
    Set.fromList
        [ "const"
        , "else"
        , "enum"
        , "false"
        , "fn"
        , "if"
        , "let"
        , "match"
        , "struct"
        , "true"
        ]

checkReservedName :: MonadError SemanticError m => Located Text -> m ()
checkReservedName n = when (unpack n `Set.member` reservedNames) (throwError (ReservedName n))

semantic :: String -> Text -> Module 'Unchecked -> Either String (Module 'Checked)
semantic filePath input = first showError . checkModule
  where
    parseErrorBundle :: Location -> String -> ParseErrorBundle Text Void
    parseErrorBundle location message = ParseErrorBundle
        { bundleErrors = NonEmpty.singleton (FancyError (offset location) (Set.singleton (ErrorFail message)))
        , bundlePosState = initialPosState
        }
      where
        initialPosState = PosState
            { pstateInput = input
            , pstateOffset = 0
            , pstateSourcePos = initialPos filePath
            , pstateTabWidth = defaultTabWidth
            , pstateLinePrefix = mempty
            }

    prettyTypeList = \case
        [] -> "<none>"
        ts -> intercalate ", " ["`" <> prettyType t <> "`" | t <- ts]

    prettyArity = \case
        1 -> "1 argument"
        n -> show n <> " arguments"

    showError = \case
        RedefinedType (Located location d) -> errorBundlePretty (parseErrorBundle location message)
          where
            message = "A type named `" <> Text.unpack (typeDefName d) <> "` was already defined earlier in this file."
        RedefinedConst (Located location d) -> errorBundlePretty (parseErrorBundle location message)
          where
            message = "A constant named `" <> Text.unpack (constDefName d) <> "` was already defined earlier in this file."
        RedefinedFn (Located location d) -> errorBundlePretty (parseErrorBundle location message)
          where
            message = "A function named `" <> Text.unpack (fnDefName d) <> "` was already defined earlier in this file."
        RedefinedEnumVariant location n v -> errorBundlePretty (parseErrorBundle location message)
          where
            message = "An enum variant named `" <> Text.unpack (n <> "::" <> v) <> "` was already defined earlier in this file."
        StructOrEnumNotFound (Located location n) -> errorBundlePretty (parseErrorBundle location message)
          where
            message = "Type `" <> Text.unpack n <> "` not found."
        TypeCycle typeCycle -> "The following types form a cycle: " <> intercalate ", " names
          where
            names = ["`" <> Text.unpack (typeDefName d) <> "`" | d <- toList typeCycle]
        StructNotFound (Located location n) -> errorBundlePretty (parseErrorBundle location message)
          where
            message = "Struct `" <> Text.unpack n <> "` not found."
        EnumNotFound (Located location n) -> errorBundlePretty (parseErrorBundle location message)
          where
            message = "Enum `" <> Text.unpack n <> "` not found."
        EnumVariantNotFound location n v -> errorBundlePretty (parseErrorBundle location message)
          where
            message = "Enum variant `" <> Text.unpack (n <> "::" <> v) <> "` not found."
        ConstNotFound (Located location n) -> errorBundlePretty (parseErrorBundle location message)
          where
            message = "Constant `" <> Text.unpack n <> "` not found."
        FnNotFound (Located location n) -> errorBundlePretty (parseErrorBundle location message)
          where
            message = "Function `" <> Text.unpack n <> "` not found."
        ValueNotFound (Located location n) -> errorBundlePretty (parseErrorBundle location message)
          where
            message = "Value `" <> Text.unpack n <> "` not found."
        ConstDefTypeMismatch location actual expected -> errorBundlePretty (parseErrorBundle location message)
          where
            message =
                intercalate
                    "\n"
                    [ "The right-hand side of this constant definition is of the wrong type."
                    , ""
                    , "Expected type:"
                    , ""
                    , "    `" <> prettyType expected <> "`"
                    , ""
                    , "Actual type:"
                    , ""
                    , "    `" <> prettyType actual <> "`"
                    , ""
                    ]
        NegLitInitTypeMismatch (Located location t') -> errorBundlePretty (parseErrorBundle location message)
          where
            message = "Type `" <> prettyType t' <> "` has no definition of `-`."
        ArgumentTypesMismatch location actual expected -> errorBundlePretty (parseErrorBundle location message)
          where
            message =
                intercalate
                    "\n"
                    [ "Incorrect argument types."
                    , ""
                    , "Expected types:"
                    , ""
                    , "    " <> prettyTypeList (toList expected)
                    , ""
                    , "Actual types:"
                    , ""
                    , "    " <> prettyTypeList (toList actual)
                    , ""
                    ]
        MainFnDefMissing -> "Main function not found."
        MainFnDefArityMismatch location arity -> errorBundlePretty (parseErrorBundle location message)
          where
            message =
                "This function has "
                <> prettyArity arity
                <> ", but the main function is required to have "
                <> prettyArity 0
                <> "."
        MainFnDefReturnTypeMismatch location returnType -> errorBundlePretty (parseErrorBundle location message)
          where
            message =
                "The declared return type of this function is `"
                <> prettyType returnType
                <> "`, but the return type of the main function is required to be `"
                <> prettyType Unit
                <> "`."
        FnDefTypeMismatch location resultType returnType -> errorBundlePretty (parseErrorBundle location message)
          where
            message =
                "This function returns a value of type `"
                <> prettyType resultType
                <> "`, but its declared return type is `"
                <> prettyType returnType
                <> "`."
        IfScrutineeTypeMismatch location actual -> errorBundlePretty (parseErrorBundle location message)
          where
            message =
                intercalate
                    "\n"
                    [ "This `if` scrutinee is of the wrong type."
                    , ""
                    , "Expected type:"
                    , ""
                    , "    `" <> prettyType Bool <> "`"
                    , ""
                    , "Actual type:"
                    , ""
                    , "    `" <> prettyType actual <> "`"
                    , ""
                    ]
        IfBlocksTypeMismatch location actual expected -> errorBundlePretty (parseErrorBundle location message)
          where
            message =
                intercalate
                    "\n"
                    [ "This block returns a value of a different type than the first block of the enclosing `if`."
                    , ""
                    , "Expected type:"
                    , ""
                    , "    `" <> prettyType expected <> "`"
                    , ""
                    , "Actual type:"
                    , ""
                    , "    `" <> prettyType actual <> "`"
                    , ""
                    ]
        MatchScrutineeTypeMismatch location t0 -> errorBundlePretty (parseErrorBundle location message)
          where
            message = "This `match` scrutinee is of type `" <> prettyType t0 <> "`, which is not an enum."
        MatchArmEnumMismatch location actual expected -> errorBundlePretty (parseErrorBundle location message)
          where
            message =
                "This `match` arm refers to an enum named `"
                <> Text.unpack actual
                <> "`, which is different from the expected `"
                <> Text.unpack expected
                <> "`."
        MatchArmEnumVariantNotFound location n v -> errorBundlePretty (parseErrorBundle location message)
          where
            message = "Enum variant `" <> Text.unpack (n <> "::" <> v) <> "` not found."
        MatchArmResultTypeMismatch location t1 t0 -> errorBundlePretty (parseErrorBundle location message)
          where
            message =
                intercalate
                    "\n"
                    [ "This block returns a value of a different type than the first block of the enclosing `match`."
                    , ""
                    , "Expected type:"
                    , ""
                    , "    `" <> prettyType t0 <> "`"
                    , ""
                    , "Actual type:"
                    , ""
                    , "    `" <> prettyType t1 <> "`"
                    , ""
                    ]
        MatchArmEnumVariantsDuplicated location n vs -> errorBundlePretty (parseErrorBundle location message)
          where
            message =
                intercalate
                    "\n"
                    [ "The following enum variants are matched by more than one arm:"
                    , ""
                    , "    " <> intercalate ", " [Text.unpack ("`" <> n <> "::" <> v <> "`") | v <- toList vs]
                    , ""
                    ]
        MatchArmEnumVariantsMissing location n vs -> errorBundlePretty (parseErrorBundle location message)
          where
            message =
                intercalate
                    "\n"
                    [ "The following enum variants are matched by zero arms:"
                    , ""
                    , "    " <> intercalate ", " [Text.unpack ("`" <> n <> "::" <> v <> "`") | v <- toList vs]
                    , ""
                    ]
        StructPatternArityMismatch location actual expected -> errorBundlePretty (parseErrorBundle location message)
          where
            message =
                "This pattern has "
                <> prettyArity actual
                <> ", but it was expected to have "
                <> prettyArity expected
                <> "."
        StructPatternTypeMismatch location actual expected -> errorBundlePretty (parseErrorBundle location message)
          where
            message =
                "The right-hand side of this pattern is of type `"
                <> prettyType actual
                <> "`, but it was expected to be of type `"
                <> prettyType expected
                <> "`."
        UnaryOpTypeMismatch location o t -> errorBundlePretty (parseErrorBundle location message)
          where
            message =
                "The operator `"
                <> prettyUnaryOp o
                <> "` is not defined for the type `"
                <> prettyType t
                <> "`."
        BinaryOpTypeMismatch location o t0 t1 -> errorBundlePretty (parseErrorBundle location message)
          where
            message =
                "The operator `"
                <> prettyBinaryOp o
                <> "` is not defined for the types `"
                <> prettyType t0
                <> "`, `"
                <> prettyType t1
                <> "`."
        PrintExprExcessHole location -> errorBundlePretty (parseErrorBundle location message)
          where
            message = "No argument was given for this hole."
        PrintExprExcessArg location -> errorBundlePretty (parseErrorBundle location message)
          where
            message = "No hole was given for this argument"
        TypeNotPrintable (Located location t) -> errorBundlePretty (parseErrorBundle location message)
          where
            message = "The type `" <> prettyType t <> "` is not printable."
        IntrinsicFn (Located location n) -> errorBundlePretty (parseErrorBundle location message)
          where
            message = "The name `" <> Text.unpack n <> "` is already taken by an intrinsic function."
        ReservedName (Located location n) -> errorBundlePretty (parseErrorBundle location message)
          where
            message = "The name `" <> Text.unpack n <> "` is reserved by the compiler."

sortAndGroupBy :: Ord key => (a -> key) -> [a] -> [[a]]
sortAndGroupBy key = groupBy ((==) `on` key) . sortOn key

checkModule :: Module 'Unchecked -> Either SemanticError (Module 'Checked)
checkModule (UncheckedModule typeDefs constDefs fnDefs) = do
    -- Check for redefined types.
    let typeDefsByName = sortAndGroupBy (typeDefName . unpack) typeDefs
    for_ typeDefsByName \case
        _ : d : _ -> throwError (RedefinedType d)
        _ -> pure ()

    -- Check for redefined constants.
    let constDefsByName = sortAndGroupBy (constDefName . unpack) constDefs
    for_ constDefsByName \case
        _ : d : _ -> throwError (RedefinedConst d)
        _ -> pure ()

    -- Check for redefined functions.
    let fnDefsByName = sortAndGroupBy (fnDefName . unpack) fnDefs
    for_ fnDefsByName \case
        _ : d : _ -> throwError (RedefinedFn d)
        _ -> pure ()

    -- Resolve all types.
    typeDefs' <- traverse resolveTypeDef (fmap unpack typeDefs)

    -- Resolve all constant declarations.
    let constDecls = [d | UncheckedConstDef d _ <- fmap unpack constDefs]
    let constInits = [c | UncheckedConstDef _ c <- fmap unpack constDefs]
    constDecls' <- traverse resolveConstDecl constDecls

    -- Resolve all function declaractions.
    let fnDecls = [d | FnDef d _ <- fmap unpack fnDefs]
    let fnBodies = [b | FnDef _ b <- fmap unpack fnDefs]
    fnDecls' <- traverse resolveFnDecl fnDecls

    -- Check that the type definition graph is acyclic.
    void (first TypeCycle (sortTypeDefs (fmap unpack typeDefs)))

    let context = Context
            { _structs = Map.fromList [(n, ts) | StructDef n ts <- typeDefs']
            , _enums = Map.fromList [(n, Map.fromList (fmap invertEnumVariant (indexed vs))) | CheckedEnumDef n vs <- typeDefs']
            , _consts = Map.fromList [(n, t) | ConstDecl n t <- constDecls']
            , _strings = mempty
            , _fns = Map.fromList (intrinsicFns <> [(n, (fmap fnArgType as, t)) | CheckedFnDecl n as t <- fnDecls'])
            , _values = mempty
            }

    -- Check all constant definitions.
    constDefs' <- evalSemantic (zipWithM checkConstDef constDecls' constInits) context

    -- Check all function definitions.
    (fnDefs', context') <- runSemantic (zipWithM checkFnDef fnDecls' fnBodies) context

    let internPool = Vector.fromList (fmap fst (sortOn snd (Map.toList (view strings context'))))

    -- Check that there is exactly one main function, of the right signature.
    let (otherFnDefs', mainFnDefs') = partition ((/= "main") . fnDefName) fnDefs'
    case mainFnDefs' of
        [] -> throwError MainFnDefMissing
        [mainFnDef'@(FnDef (CheckedFnDecl _ as' returnType) _)] -> do
            when (arity /= 0) (throwError (MainFnDefArityMismatch arityLocation arity))
            when (returnType /= Unit) (throwError (MainFnDefReturnTypeMismatch returnTypeLocation returnType))
            pure (CheckedModule typeDefs' constDefs' internPool otherFnDefs' mainFnDef')
          where
            arity = Vector.length as'

            fnDeclsByName = Map.fromList [(fnDeclName d, d) | d <- fnDecls]

            UncheckedFnDecl _ (Located arityLocation _) (Located returnTypeLocation _) = fnDeclsByName Map.! "main"
        _ -> error ("Internal error: expected at most one main function, got `" <> show mainFnDefs' <> "`.")
  where
    structNames = Set.fromList [n | StructDef n _ <- fmap unpack typeDefs]

    enumNames = Set.fromList [n | UncheckedEnumDef n _ <- fmap unpack typeDefs]

    resolveType = \case
        Unit -> pure Unit
        Bool -> pure Bool
        I64 -> pure I64
        F64 -> pure F64
        StructOrEnum n
            | isStruct && isEnum -> error ("Internal error: `" <> Text.unpack (unpack n) <> "` is both a struct and an enum.")
            | isStruct -> pure (Struct (unpack n))
            | isEnum -> pure (Enum (unpack n))
            | otherwise -> throwError (StructOrEnumNotFound n)
          where
            isStruct = unpack n `Set.member` structNames

            isEnum = unpack n `Set.member` enumNames

    resolveTypeDef = \case
        StructDef n ts -> do
            ts' <- traverse resolveType ts
            pure (StructDef n ts')
        UncheckedEnumDef n vs -> do
            for_ (sortAndGroupBy (enumVariantName . unpack) vs) \case
                _ : v : _ -> throwError (RedefinedEnumVariant (locate v) n (enumVariantName (unpack v)))
                _ -> pure ()
            vs' <- traverse (resolveEnumVariant . unpack) vs
            pure (CheckedEnumDef n vs')

    resolveEnumVariant (EnumVariant v ts) = do
        ts' <- traverse resolveType ts
        pure (EnumVariant v ts')

    resolveConstDecl (ConstDecl x t) = do
        constDeclType <- resolveType t
        pure (ConstDecl x constDeclType)

    resolveFnDecl (UncheckedFnDecl n as t) = do
        checkIntrinsicFn n
        checkReservedName n
        as' <- traverse resolveFnArg (unpack as)
        t' <- resolveType (unpack t)
        pure (CheckedFnDecl (unpack n) as' t')

    resolveFnArg (UncheckedFnArg an at) = do
        checkReservedName an
        at' <- resolveType at
        pure (CheckedFnArg (unpack an) at')

sortTypeDefs :: [TypeDef 'Unchecked] -> Either (NonEmpty (TypeDef 'Unchecked)) [TypeDef 'Unchecked]
sortTypeDefs typeDefs = bimap (fmap fromLabel) (fmap fromLabel) (topSort graph)
  where
    toLabel = (Map.fromList [(typeDefName d, i) | (i, d) <- indexed typeDefs] Map.!)

    fromLabel = (Map.fromList (indexed typeDefs) Map.!)

    neighbors = \case
        StructDef _ ts -> [toLabel (unpack n) | StructOrEnum n <- toList ts]
        UncheckedEnumDef _ vs -> [toLabel (unpack n) | EnumVariant _ ts <- fmap unpack vs, StructOrEnum n <- toList ts]

    graph = stars [(toLabel (typeDefName d), neighbors d) | d <- typeDefs]

bindFnArg :: FnArg 'Checked -> Semantic ()
bindFnArg (CheckedFnArg an at) = bindValue an at

checkFnDef :: FnDecl 'Checked -> Block 'Unchecked -> Semantic (FnDef 'Checked)
checkFnDef d@(CheckedFnDecl _ as returnType) b = do
    (b', resultType) <- namespaced do
        traverse_ bindFnArg as
        checkBlock b
    when (resultType /= returnType) (throwError (FnDefTypeMismatch (locateBlock b) resultType returnType))
    pure (FnDef d b')

checkBlock :: Block 'Unchecked -> Semantic (Block 'Checked, Type 'Checked)
checkBlock (UncheckedBlock ss e) = do
    ss' <- traverse (checkStatement . unpack) ss
    (e', t) <- checkExpr (unpack e)
    pure (CheckedBlock ss' e', t)

checkStatement :: Statement 'Unchecked -> Semantic (Statement 'Checked)
checkStatement = \case
    UncheckedLetStatement (UncheckedVarPattern x) e -> do
        checkReservedName x
        (e', t) <- checkExpr e
        bindValue (unpack x) t
        pure (CheckedLetStatement (CheckedVarPattern (unpack x)) e')
    UncheckedLetStatement (UncheckedStructPattern n xs) e -> do
        traverse_ checkReservedName (unpack xs)
        fieldTypes <- findStruct n
        let actualArity = Vector.length (unpack xs)
        let expectedArity = Vector.length fieldTypes
        when (actualArity /= expectedArity) (throwError (StructPatternArityMismatch (locate xs) actualArity expectedArity))
        (e', actualType) <- checkExpr e
        let expectedType = Struct (unpack n)
        when (actualType /= expectedType) (throwError (StructPatternTypeMismatch (locateExpr e) actualType expectedType))
        let xts = Vector.zip (fmap unpack (unpack xs)) fieldTypes
        traverse_ (uncurry bindValue) xts
        pure (CheckedLetStatement (CheckedStructPattern xts) e')
    UncheckedExprStatement e -> do
        (e', _) <- checkExpr e
        pure (CheckedExprStatement e')

checkExpr :: Expr 'Unchecked -> Semantic (Expr 'Checked, Type 'Checked)
checkExpr = \case
    UncheckedExprWithBlock e -> checkExprWithBlock e
    UncheckedExprWithoutBlock e -> checkExprWithoutBlock (unpack e)

checkExprWithBlock :: UncheckedExprWithBlock -> Semantic (Expr 'Checked, Type 'Checked)
checkExprWithBlock = \case
    UncheckedBlockExpr b -> fmap (first CheckedBlockExpr) (checkBlock b)
    UncheckedIfExpr e b0 b1 -> do
        (e', t) <- checkExprWithoutBlock (unpack e)
        when (t /= Bool) (throwError (IfScrutineeTypeMismatch (locate e) t))
        (b0', t0) <- checkBlock b0
        (b1', t1) <- checkBlock b1
        when (t1 /= t0) (throwError (IfBlocksTypeMismatch (locateBlock b1) t1 t0))
        pure (CheckedIfExpr e' b0' b1', t0)
    UncheckedMatchExpr _ e0 uncheckedMatchArms -> do
        (e0', t0) <- checkExprWithoutBlock (unpack e0)
        case t0 of
            Enum n0 -> do
                vs0 <- fmap (Map.! n0) (use enums)

                checkedMatchArms <- traverse (checkMatchArm n0 vs0) uncheckedMatchArms

                let coverageByName = matchCoverage vs0 checkedMatchArms

                case NonEmpty.nonEmpty [v0 | (v0, n) <- coverageByName, n > 1] of
                    Nothing -> pure ()
                    Just actual -> throwError (MatchArmEnumVariantsDuplicated (locate e0) n0 actual)

                case NonEmpty.nonEmpty [v0 | (v0, 0) <- coverageByName] of
                    Nothing -> pure ()
                    Just actual -> throwError (MatchArmEnumVariantsMissing (locate e0) n0 actual)

                case NonEmpty.nonEmpty checkedMatchArms of
                    Nothing -> pure (CheckedLitExpr UnitLit, Unit)
                    Just branches -> do
                        resultType <- matchResultType branches
                        let sortedBranches = fmap cmacMatchArm (NonEmpty.sortWith cmacEnumVariantIndex branches)
                        pure (CheckedMatchExpr e0' n0 sortedBranches, resultType)
            _ -> throwError (MatchScrutineeTypeMismatch (locate e0) t0)

data CheckedMatchArmContext = CheckedMatchArmContext
    { cmacMatchArm :: MatchArm 'Checked
    , cmacResultLocation :: Location
    , cmacResultType :: Type 'Checked
    , cmacEnumVariantIndex :: Int
    }

checkMatchArm :: Text -> Map Text EnumVariantContext -> MatchArm 'Unchecked -> Semantic CheckedMatchArmContext
checkMatchArm n0 vs0 (UncheckedMatchArm n v xs e) = do
    when (unpack n /= n0) (throwError (MatchArmEnumMismatch (locate n) (unpack n) n0))
    case Map.lookup (unpack v) vs0 of
        Nothing -> throwError (MatchArmEnumVariantNotFound (locate v) n0 (unpack v))
        Just (EnumVariantContext i fieldTypes) -> do
            traverse_ checkReservedName (unpack xs)
            let actual = Vector.length (unpack xs)
            let expected = Vector.length fieldTypes
            when (actual /= expected) (throwError (StructPatternArityMismatch (locate xs) actual expected))
            namespaced do
                let xts = Vector.zip (fmap unpack (unpack xs)) fieldTypes
                traverse_ (uncurry bindValue) xts
                (e', resultType) <- checkExpr e
                pure CheckedMatchArmContext
                    { cmacMatchArm = CheckedMatchArm xts e'
                    , cmacResultLocation = locateExpr e
                    , cmacResultType = resultType
                    , cmacEnumVariantIndex = i
                    }

histogram :: Ord a => [a] -> Map a Int
histogram = Map.fromListWith (+) . fmap (, 1)

occurrences :: Ord a => a -> Map a Int -> Int
occurrences = Map.findWithDefault 0

matchCoverage :: Map Text EnumVariantContext -> [CheckedMatchArmContext] -> [(Text, Int)]
matchCoverage vs0 checkedMatchArms = coverageByName
  where
    coverageByIndex = (`occurrences` histogram (fmap cmacEnumVariantIndex checkedMatchArms))

    coverageByName = Map.toList (Map.map (coverageByIndex . enumVariantIndex) vs0)

matchResultType :: MonadError SemanticError m => NonEmpty CheckedMatchArmContext -> m (Type 'Checked)
matchResultType (CheckedMatchArmContext {cmacResultType = expected} :| checkedMatchArms) =
    case filter ((/= expected) . cmacResultType) checkedMatchArms of
        [] -> pure expected
        CheckedMatchArmContext {cmacResultLocation = location, cmacResultType = actual} : _ -> throwError (MatchArmResultTypeMismatch location actual expected)

formatSpecifier :: Located (Type 'Checked) -> Semantic Builder
formatSpecifier t =
    case unpack t of
        I64 -> pure "%lld"
        F64 -> pure "%f"
        _ -> throwError (TypeNotPrintable t)

checkExprWithoutBlock :: UncheckedExprWithoutBlock -> Semantic (Expr 'Checked, Type 'Checked)
checkExprWithoutBlock = \case
    UncheckedLitExpr l -> pure (CheckedLitExpr l, litType l)
    UncheckedVarExpr x -> do
        t <- findValue x
        pure (CheckedVarExpr (unpack x) t, t)
    UncheckedConstExpr x -> do
        t <- findConst x
        pure (CheckedConstExpr (unpack x) t, t)
    UncheckedUnaryOpExpr o e -> do
        (e', t) <- checkExprWithoutBlock e
        case (unpack o, t) of
            (Neg, I64) -> pure (CheckedUnaryOpExpr Neg e', I64)
            (Neg, F64) -> pure (CheckedUnaryOpExpr FNeg e', F64)
            (Not, Bool) -> pure (CheckedUnaryOpExpr Not e', Bool)
            (As F64, I64) -> pure (CheckedUnaryOpExpr AsF64 e', F64)
            (As I64, F64) -> pure (CheckedUnaryOpExpr AsI64 e', I64)
            _ -> throwError (UnaryOpTypeMismatch (locate o) (unpack o) t)
    UncheckedBinaryOpExpr o e0 e1 -> do
        (e0', t0) <- checkExprWithoutBlock e0
        (e1', t1) <- checkExprWithoutBlock e1
        case (unpack o, t0, t1) of
            (Mul, I64, I64) -> pure (CheckedBinaryOpExpr Mul e0' e1', I64)
            (Mul, F64, F64) -> pure (CheckedBinaryOpExpr FMul e0' e1', F64)
            (Div, I64, I64) -> pure (CheckedBinaryOpExpr Div e0' e1', I64)
            (Div, F64, F64) -> pure (CheckedBinaryOpExpr FDiv e0' e1', F64)
            (Rem, I64, I64) -> pure (CheckedBinaryOpExpr Rem e0' e1', I64)
            (Rem, F64, F64) -> pure (CheckedBinaryOpExpr FRem e0' e1', F64)
            (Add, I64, I64) -> pure (CheckedBinaryOpExpr Add e0' e1', I64)
            (Add, F64, F64) -> pure (CheckedBinaryOpExpr FAdd e0' e1', F64)
            (Sub, I64, I64) -> pure (CheckedBinaryOpExpr Sub e0' e1', I64)
            (Sub, F64, F64) -> pure (CheckedBinaryOpExpr FSub e0' e1', F64)
            (Shl, I64, I64) -> pure (CheckedBinaryOpExpr Shl e0' e1', I64)
            (Shr, I64, I64) -> pure (CheckedBinaryOpExpr Shr e0' e1', I64)
            (BitAnd, I64, I64) -> pure (CheckedBinaryOpExpr BitAnd e0' e1', I64)
            (BitXor, I64, I64) -> pure (CheckedBinaryOpExpr BitXor e0' e1', I64)
            (BitOr, I64, I64) -> pure (CheckedBinaryOpExpr BitOr e0' e1', I64)
            (Cmp p, I64, I64) -> pure (CheckedBinaryOpExpr (Icmp p) e0' e1', Bool)
            (Cmp p, F64, F64) -> pure (CheckedBinaryOpExpr (Fcmp p) e0' e1', Bool)
            (And, Bool, Bool) -> pure (CheckedBinaryOpExpr And e0' e1', Bool)
            (Or, Bool, Bool) -> pure (CheckedBinaryOpExpr Or e0' e1', Bool)
            _ -> throwError (BinaryOpTypeMismatch (locate o) (unpack o) t0 t1)
    UncheckedCallExpr n es -> do
        ets' <- traverse checkExprWithoutBlock (unpack es)
        let ts = fmap snd ets'
        (argumentTypes, returnType) <- findFn n
        when (ts /= argumentTypes) (throwError (ArgumentTypesMismatch (locate es) ts argumentTypes))
        pure (CheckedCallExpr (unpack n) (fmap fst ets') returnType, returnType)
    UncheckedStructExpr n es -> do
        ets' <- traverse checkExprWithoutBlock (unpack es)
        let ts = fmap snd ets'
        fieldTypes <- findStruct n
        when (ts /= fieldTypes) (throwError (ArgumentTypesMismatch (locate es) ts fieldTypes))
        pure (CheckedStructExpr (unpack n) ets', Struct (unpack n))
    UncheckedEnumExpr n v es -> do
        ets' <- traverse checkExprWithoutBlock (unpack es)
        let ts = fmap snd ets'
        EnumVariantContext i fieldTypes <- findEnumVariant n v
        when (ts /= fieldTypes) (throwError (ArgumentTypesMismatch (locate es) ts fieldTypes))
        pure (CheckedEnumExpr (unpack n) i ets', Enum (unpack n))
    UncheckedPrintExpr (UncheckedFormatString cs) es -> do
        ets' <- traverse (traverse checkExprWithoutBlock) es

        let ts = fmap (fmap snd) ets'

        builder <- loop mempty cs ts

        let f = Text.Lazy.toStrict (Text.Lazy.Builder.toLazyText builder)

        i <- bindString f

        let es' = fmap (fst . unpack) ets'

        pure (CheckedPrintExpr (CheckedFormatString i) es', Unit)
      where
        loop builder chunks types =
            let (builder', chunks') = nonHoles builder chunks in

            hole builder' chunks' types

        nonHoles builder chunks =
            let (prefix, chunks') = span ((/= Hole) . unpack) chunks in

            let builder' = builder <> mconcat [Text.Lazy.Builder.fromText x | NonHole x <- fmap unpack prefix] in

            (builder', chunks')

        hole builder chunks types =
            case (chunks, types) of
                -- There are neither holes nor arguments left, so we are done.
                ([], []) -> pure builder
                -- There are too many holes.
                (c : _, []) -> throwError (PrintExprExcessHole (locate c))
                -- There are too many arguments.
                ([], t : _) -> throwError (PrintExprExcessArg (locate t))
                -- There are both holes and arguments left.
                (_ : chunks', t : types') -> do
                    s <- formatSpecifier t
                    loop (builder <> s) chunks' types'

checkConstDef :: ConstDecl 'Checked -> Located (ConstInit 'Unchecked) -> Semantic (ConstDef 'Checked)
checkConstDef d@(ConstDecl _ constDeclType) c = do
    (c', t) <- checkConstInit (unpack c)
    when (t /= constDeclType) (throwError (ConstDefTypeMismatch (locate c) t constDeclType))
    pure (CheckedConstDef d c')

checkConstInit :: ConstInit 'Unchecked -> Semantic (ConstInit 'Checked, Type 'Checked)
checkConstInit = \case
    LitInit l -> pure (LitInit l, litType l)
    UncheckedNegLitInit l ->
        case unpack l of
            I64Lit x -> pure (NegI64LitInit x, I64)
            F64Lit x -> pure (NegF64LitInit x, F64)
            _ -> throwError (NegLitInitTypeMismatch (fmap litType l))
    UncheckedStructInit n cs -> do
        fieldTypes <- findStruct n
        cts' <- traverse checkConstInit (unpack cs)
        let ts = fmap snd cts'
        when (ts /= fieldTypes) (throwError (ArgumentTypesMismatch (locate cs) ts fieldTypes))
        pure (CheckedStructInit (unpack n) (fmap fst cts'), Struct (unpack n))
    UncheckedEnumInit n v cs -> do
        EnumVariantContext i fieldTypes <- findEnumVariant n v
        cts' <- traverse checkConstInit (unpack cs)
        let ts = fmap snd cts'
        when (ts /= fieldTypes) (throwError (ArgumentTypesMismatch (locate cs) ts fieldTypes))
        pure (CheckedEnumInit (unpack n) i (fmap fst cts'), Enum (unpack n))
