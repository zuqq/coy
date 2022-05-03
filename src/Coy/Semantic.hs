{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coy.Semantic where

import Algebra.Graph (stars)
import Algebra.Graph.ToGraph (topSort)
import Control.Monad (void, when, zipWithM)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Trans.Except (Except, runExcept)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, runStateT)
import Data.Bifunctor (bimap, first)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (for_, toList, traverse_)
import Data.List (partition, sort, sortOn)
import Data.List.Index (indexed)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Traversable (for)
import Data.Vector (Vector)
import Lens.Micro (Lens', _1, _2, _3, lens)
import Lens.Micro.Mtl ((%=), (.=), use, view)
import Text.Megaparsec

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Lazy.Builder
import qualified Data.Vector as Vector

import Coy.Syntax

data Context = Context
    { _structs :: Map Text (Vector (Type 'Checked))
    , _enums :: Map Text (Map Text (Int, Vector (Type 'Checked)))
    , _consts :: Map Text (Type 'Checked)
    , _strings :: Map Text Int
    , _fns :: Map Text (Vector (Type 'Checked), Type 'Checked)
    , _values :: Map Text (Type 'Checked)
    }
    deriving Show

structs :: Lens' Context (Map Text (Vector (Type 'Checked)))
structs = lens _structs (\c ss -> c {_structs = ss})

enums :: Lens' Context (Map Text (Map Text (Int, Vector (Type 'Checked))))
enums = lens _enums (\c es -> c {_enums = es})

consts :: Lens' Context (Map Text (Type 'Checked))
consts = lens _consts (\c cs -> c {_consts = cs})

strings :: Lens' Context (Map Text Int)
strings = lens _strings (\c ss -> c {_strings = ss})

fns :: Lens' Context (Map Text (Vector (Type 'Checked), Type 'Checked))
fns = lens _fns (\c fs -> c {_fns = fs})

values :: Lens' Context (Map Text (Type 'Checked))
values = lens _values (\c vs -> c {_values = vs})

data SemanticError
    = RedefinedType (TypeDef 'Unchecked)
    | RedefinedFn (FnDef 'Unchecked)
    | StructOrEnumNotFound Text
    | TypeCycle (NonEmpty (TypeDef 'Unchecked))
    | StructNotFound Text
    | EnumNotFound Text
    | EnumVariantNotFound Text Text
    | ConstNotFound Text
    | FnNotFound Text
    | ValueNotFound Text
    | ConstDefTypeMismatch
    -- ^ The declared and observed type of a constant differ.
        (ConstDecl 'Checked)
        -- ^ Left-hand side of the constant definition.
        (ConstInit 'Unchecked)
        -- ^ Right-hand side of the constant definition.
        (Type 'Checked)
        -- ^ Observed type.
    | NegLitInitTypeMismatch Lit (Type 'Checked)
    | StructInitTypeMismatch
    -- ^ The given argument list doesn't conform to the constructor's signature.
        Text
        -- ^ Name of the struct.
        (Vector (ConstInit 'Unchecked))
        -- ^ Arguments.
        (Vector (Type 'Checked))
        -- ^ Types of the arguments.
    | EnumInitTypeMismatch
    -- ^ The given argument list doesn't conform to the constructor's signature.
        Text
        -- ^ Name of the enum.
        Text
        -- ^ Name of the enum variant.
        (Vector (ConstInit 'Unchecked))
        -- ^ Arguments.
        (Vector (Type 'Checked))
        -- ^ Types of the arguments.
    | MainFnDefMissing
    | MainFnDefTypeMismatch (FnDef 'Checked)
    | FnDefTypeMismatch
    -- ^ The declared and observed return type of a function differ.
        Text
        -- ^ Name of the function.
        (Type 'Checked)
        -- ^ Declared return type.
        (Type 'Checked)
        -- ^ Observed return type.
    | IfScrutineeTypeMismatch
    -- ^ The scrutinee in an if expression is not of type @Bool@.
        (ExprWithoutBlock 'Unchecked)
        -- ^ The scrutinee.
        (Type 'Checked)
        -- ^ Actual type of the scrutinee.
    | IfBlocksTypeMismatch
    -- ^ The result types in an if expression are not the same.
        (Block 'Unchecked)
        -- ^ The first block.
        (Type 'Checked)
        -- ^ Result type of the first block.
        (Block 'Unchecked)
        -- ^ The second block.
        (Type 'Checked)
        -- ^ Result type of the second block.
    | MatchScrutineeTypeMismatch
    -- ^ The scrutinee in a match expression is not of enum type.
        (ExprWithoutBlock 'Unchecked)
        -- ^ The scrutinee.
        (Type 'Checked)
        -- ^ Type of the scrutinee.
    | MatchArmEnumMismatch
    -- ^ The enum names in a match expression are not consistent with the type
    -- of the scrutinee.
        Text
        -- ^ Name of the scrutinee's type.
        [Text]
        -- ^ Enum names in the match arms.
    | MatchArmEnumVariantMismatch
    -- ^ The enum variant names in a match expression are not consistent with
    -- the type of the scrutinee.
        Text
        -- ^ Name of the scrutinee's type.
        [Text]
        -- ^ Enum variant names in the match arms.
    | MatchArmArityMismatch
    -- ^ The number of variables in a match arm is not equal to the arity of
    -- the corresponding constructor.
        Text
        -- ^ Name of the enum.
        Text
        -- ^ Name of the enum variant.
        (Vector Text)
        -- ^ Variables for the components.
    | MatchArmTypeMismatch
    -- ^ The result types in a match expression are not all the same.
        [MatchArm 'Unchecked]
        -- ^ The match arms.
        [Type 'Checked]
        -- ^ Result types of the match arms.
    | StructPatternArityMismatch
    -- ^ The number of variables in a struct pattern is not equal to the arity
    -- of the struct's constructor.
        Text
        -- ^ Name of the struct.
        (Vector Text)
        -- ^ Variables for the components.
    | StructPatternTypeMismatch
    -- ^ The right-hand side of a destructuring is of the wrong type.
        Text
        -- ^ Name of the struct.
        (Expr 'Unchecked)
        -- ^ Right-hand side of the destructuring.
        (Type 'Checked)
        -- ^ Type of the right-hand side.
    | UnaryOpTypeMismatch
    -- ^ The given unary operator has no overloading consistent with the
    -- arguments' types.
        (UnaryOp 'Unchecked)
        -- ^ The unary operator.
        (ExprWithoutBlock 'Unchecked)
        -- ^ Argument.
        (Type 'Checked)
        -- ^ Type of the argument.
    | BinaryOpTypeMismatch
    -- ^ The given binary operator has no overloading consistent with the
    -- arguments' types.
        (BinaryOp 'Unchecked)
        -- ^ The binary operator.
        (ExprWithoutBlock 'Unchecked)
        -- ^ First argument.
        (Type 'Checked)
        -- ^ Type of the first argument.
        (ExprWithoutBlock 'Unchecked)
        -- ^ Second argument.
        (Type 'Checked)
        -- ^ Type of the second argument.
    | CallExprTypeMismatch
    -- ^ The given argument list doesn't conform to the function's signature.
        Text
        -- ^ Name of the function.
        (Vector (ExprWithoutBlock 'Unchecked))
        -- ^ Arguments.
        (Vector (Type 'Checked))
        -- ^ Types of the arguments.
    | StructExprTypeMismatch
    -- ^ The given argument list doesn't conform to the constructor's signature.
        Text
        -- ^ Name of the struct.
        (Vector (ExprWithoutBlock 'Unchecked))
        -- ^ Arguments.
        (Vector (Type 'Checked))
        -- ^ Types of the arguments.
    | EnumExprTypeMismatch
    -- ^ The given argument list doesn't conform to the constructor's signature.
        Text
        -- ^ Name of the enum.
        Text
        -- ^ Name of the enum variant.
        (Vector (ExprWithoutBlock 'Unchecked))
        -- ^ Arguments.
        (Vector (Type 'Checked))
        -- ^ Types of the arguments.
    | PrintLnExprArityMismatch
    -- ^ The number of holes in the format string is not equal to the length of
    -- the argument list.
        (FormatString 'Unchecked)
        -- ^ The format string.
        [ExprWithoutBlock 'Unchecked]
        -- ^ The argument list.
    | PrintLnExprExcessHole FormatStringChunk
    | PrintLnExprExcessArg (Type 'Checked)
    | PrintLnExprTypeMismatch (Type 'Checked)
    deriving (Eq, Ord, Show)

instance ShowErrorComponent SemanticError where
    showErrorComponent = show

type Semantic = StateT Context (Except SemanticError)

evalSemantic :: Semantic a -> Context -> Either SemanticError a
evalSemantic c = runExcept . evalStateT c

runSemantic :: Semantic a -> Context -> Either SemanticError (a, Context)
runSemantic c = runExcept . runStateT c

orFail :: Maybe a -> SemanticError -> Semantic a
orFail x e = maybe (throwError e) pure x

findStruct :: Text -> Semantic (Vector (Type 'Checked))
findStruct n = do
    ss <- use structs
    Map.lookup n ss `orFail` StructNotFound n

findEnum :: Text -> Semantic (Map Text (Int, Vector (Type 'Checked)))
findEnum n = do
    es <- use enums
    Map.lookup n es `orFail` EnumNotFound n

findEnumVariant :: Text -> Text -> Semantic (Int, Vector (Type 'Checked))
findEnumVariant n v = do
    e <- findEnum n
    Map.lookup v e `orFail` EnumVariantNotFound n v

findConst :: Text -> Semantic (Type 'Checked)
findConst n = do
    cs <- use consts
    Map.lookup n cs `orFail` ConstNotFound n

bindString :: Text -> Semantic Int
bindString s = do
    ss <- use strings
    let i = Map.findWithDefault (Map.size ss) s ss
    strings .= Map.insert s i ss
    pure i

findFn :: Text -> Semantic (Vector (Type 'Checked), Type 'Checked)
findFn n = do
    fs <- use fns
    Map.lookup n fs `orFail` FnNotFound n

findValue :: Text -> Semantic (Type 'Checked)
findValue x = do
    vs <- use values
    Map.lookup x vs `orFail` ValueNotFound x

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

semantic :: String -> Text -> Module 'Unchecked -> Either String (Module 'Checked)
semantic filePath s = first (errorBundlePretty . wrap) . checkModule
  where
    initialPosState = PosState
        { pstateInput = s
        , pstateOffset = 0
        , pstateSourcePos = initialPos filePath
        , pstateTabWidth = defaultTabWidth
        , pstateLinePrefix = mempty
        }

    wrap e = ParseErrorBundle
        { bundleErrors = NonEmpty.singleton (FancyError 0 (Set.singleton (ErrorCustom e)))
        , bundlePosState = initialPosState
        }

checkModule :: Module 'Unchecked -> Either SemanticError (Module 'Checked)
checkModule (UncheckedModule typeDefs constDefs fnDefs) = do
    -- Check for redefined types.
    let typeDefsByName = groupBy typeDefName typeDefs

    for_ typeDefsByName \(_, ds) ->
        case ds of
            _ : d : _ -> throwError (RedefinedType d)
            _ -> pure ()

    -- Check for redefined functions.
    let fnDefsByName = groupBy fnDefName fnDefs

    for_ fnDefsByName \(_, ds) ->
        case ds of
            _ : d : _ -> throwError (RedefinedFn d)
            _ -> pure ()

    -- Resolve all types.
    typeDefs' <- traverse resolveTypeDef typeDefs

    let constDecls = [d | ConstDef d _ <- constDefs]

    let constInits = [c | ConstDef _ c <- constDefs]

    constDecls' <- traverse resolveConstDecl constDecls

    let fnDecls = [d | FnDef d _ <- fnDefs]

    let fnBodies = [b | FnDef _ b <- fnDefs]

    fnDecls' <- traverse resolveFnDecl fnDecls

    -- Check that the type definition graph is acyclic.
    void (first TypeCycle (sortTypeDefs typeDefs))

    let context = Context
            { _structs = Map.fromList [(n, ts) | StructDef n ts <- typeDefs']
            , _enums = Map.fromList [(n, enumVariants vs) | EnumDef n vs <- typeDefs']
            , _consts = Map.fromList [(n, t) | ConstDecl n t <- constDecls']
            , _strings = mempty
            , _fns = Map.fromList (intrinsicFns <> [(n, (fmap fnArgType as, t)) | FnDecl n as t <- fnDecls'])
            , _values = mempty
            }

    constDefs' <- evalSemantic (zipWithM constDef constDecls' constInits) context

    (fnDefs', context') <- runSemantic (zipWithM fnDef fnDecls' fnBodies) context

    let internPool = Vector.fromList (fmap fst (sortOn snd (Map.toList (view strings context'))))

    let (otherFnDefs', mainFnDefs') = partition ((/= "main") . fnDefName) fnDefs'

    case mainFnDefs' of
        [] -> throwError MainFnDefMissing
        [mainFnDef'@(FnDef (FnDecl _ as t) _)]
            | Vector.null as, t == Unit -> pure (CheckedModule typeDefs' constDefs' internPool otherFnDefs' mainFnDef')
            | otherwise -> throwError (MainFnDefTypeMismatch mainFnDef')
        _ -> error ("Internal error: expected a single main function, got `" <> show mainFnDefs' <> "`.")
  where
    groupBy key = Map.toList . fmap ($ []) . Map.fromListWith (.) . fmap adapt
      where
        adapt value = (key value, (value :))

    structNames = Set.fromList [n | StructDef n _ <- typeDefs]

    enumNames = Set.fromList [n | EnumDef n _ <- typeDefs]

    resolveType = \case
        Unit -> pure Unit
        Bool -> pure Bool
        I64 -> pure I64
        F64 -> pure F64
        StructOrEnum n
            | isStruct && isEnum -> error ("Internal error: `" <> Text.unpack n <> "` is both a struct and an enum.")
            | isStruct -> pure (Struct n)
            | isEnum -> pure (Enum n)
            | otherwise -> throwError (StructOrEnumNotFound n)
          where
            isStruct = n `Set.member` structNames

            isEnum = n `Set.member` enumNames

    resolveTypeDef = \case
        StructDef n ts -> do
            ts' <- traverse resolveType ts
            pure (StructDef n ts')
        EnumDef n vs -> do
            vs' <- traverse resolveEnumVariant vs
            pure (EnumDef n vs')
      where
        resolveEnumVariant (EnumVariant v ts) = do
            ts' <- traverse resolveType ts
            pure (EnumVariant v ts')

    resolveConstDecl (ConstDecl x t) = do
        constDeclType <- resolveType t
        pure (ConstDecl x constDeclType)

    resolveFnDecl (FnDecl n as t) = do
        as' <- traverse resolveFnArg as
        t' <- resolveType t
        pure (FnDecl n as' t')
      where
        resolveFnArg (FnArg an at) = do
            at' <- resolveType at
            pure (FnArg an at')

    enumVariants vs = Map.fromList [(v, (i, ts)) | (i, EnumVariant v ts) <- indexed vs]

-- This check comes after name resolution, because it uses the unsafe `Map.!`
-- operator to look up labels.
sortTypeDefs :: [TypeDef 'Unchecked] -> Either (NonEmpty (TypeDef 'Unchecked)) [TypeDef 'Unchecked]
sortTypeDefs typeDefs = bimap (fmap fromLabel) (fmap fromLabel) (topSort graph)
  where
    toLabel = (Map.fromList [(typeDefName d, i) | (i, d) <- indexed typeDefs] Map.!)

    fromLabel = (Map.fromList (indexed typeDefs) Map.!)

    neighbors = \case
        StructDef _ ts -> [toLabel n | StructOrEnum n <- toList ts]
        EnumDef _ vs -> [toLabel n | EnumVariant _ ts <- vs, StructOrEnum n <- toList ts]

    graph = stars [(toLabel (typeDefName d), neighbors d) | d <- typeDefs]

fnDef :: FnDecl 'Checked -> Block 'Unchecked -> Semantic (FnDef 'Checked)
fnDef d@(FnDecl n as returnType) b = do
    (b', resultType) <- namespaced (do
        traverse_ bindFnArg as
        block b)
    if returnType == resultType then
        pure (FnDef d b')
    else
        throwError (FnDefTypeMismatch n returnType resultType)
  where
    bindFnArg (FnArg an at) = bindValue an at

block :: Block 'Unchecked -> Semantic (Block 'Checked, Type 'Checked)
block (Block ss e) = do
    ss' <- traverse statement ss
    (e', t) <- expr e
    pure (Block ss' e', t)

statement :: Statement 'Unchecked -> Semantic (Statement 'Checked)
statement = \case
    LetStatement (VarPattern x) e -> do
        (e', t) <- expr e
        bindValue x t
        pure (LetStatement (VarPattern x) e')
    LetStatement (UncheckedStructPattern n xs) e -> do
        fieldTypes <- findStruct n
        when
            (Vector.length xs /= Vector.length fieldTypes)
            (throwError (StructPatternArityMismatch n xs))
        (e', t) <- expr e
        when
            (Struct n /= t)
            (throwError (StructPatternTypeMismatch n e t))
        let xts = Vector.zip xs fieldTypes
        traverse_ (uncurry bindValue) xts
        pure (LetStatement (CheckedStructPattern xts) e')
    ExprStatement e -> do
        (e', _) <- expr e
        pure (ExprStatement e')

expr :: Expr 'Unchecked -> Semantic (Expr 'Checked, Type 'Checked)
expr = \case
    ExprWithBlock e -> fmap (first ExprWithBlock) (exprWithBlock e)
    ExprWithoutBlock e -> fmap (first ExprWithoutBlock) (exprWithoutBlock e)

exprWithBlock
    :: ExprWithBlock 'Unchecked
    -> Semantic (ExprWithBlock 'Checked, Type 'Checked)
exprWithBlock = \case
    BlockExpr b -> fmap (first BlockExpr) (block b)
    IfExpr e b0 b1 -> do
        (e', t) <- exprWithoutBlock e
        when (t /= Bool) (throwError (IfScrutineeTypeMismatch e t))
        (b0', t0) <- block b0
        (b1', t1) <- block b1
        when (t0 /= t1) (throwError (IfBlocksTypeMismatch b0 t0 b1 t1))
        pure (IfExpr e' b0' b1', t0)
    UncheckedMatchExpr e0 as -> do
        (e0', t0) <- exprWithoutBlock e0
        case t0 of
            Enum n -> do
                vs <- fmap Map.keysSet (findEnum n)

                -- Check that the enum names are all equal to @n@.
                let actualEnums = [n' | UncheckedMatchArm n' _ _ _ <- as]
                when
                    (Set.fromList actualEnums /= Set.singleton n)
                    (throwError (MatchArmEnumMismatch n actualEnums))

                -- Check that the enum variant names exactly cover the variants.
                let actualEnumVariants = [v | UncheckedMatchArm _ v _ _ <- as]
                when
                    (sort actualEnumVariants /= Set.toAscList vs)
                    (throwError
                        (MatchArmEnumVariantMismatch n actualEnumVariants))

                iats' <- for as (\(UncheckedMatchArm _ v xs e) -> do
                    (i, fieldTypes) <- findEnumVariant n v
                    if Vector.length xs == Vector.length fieldTypes then
                        namespaced (do
                            let xts = Vector.zip xs fieldTypes
                            traverse_ (uncurry bindValue) xts
                            (e', resultType) <- expr e
                            pure (i, CheckedMatchArm xts e', resultType))
                    else
                        throwError (MatchArmArityMismatch n v xs))

                -- Check that the types of the match arms are all the same.
                let resultTypes = fmap (view _3) iats'
                case nubOrd resultTypes of
                    [resultType] ->
                        -- Sort the checked match arms by variant index.
                        let as' = fmap (view _2) (sortOn (view _1) iats') in

                        pure (CheckedMatchExpr e0' n as', resultType)
                    _ ->
                        throwError
                            (MatchArmTypeMismatch as resultTypes)
            _ -> throwError (MatchScrutineeTypeMismatch e0 t0)

exprWithoutBlock
    :: ExprWithoutBlock 'Unchecked
    -> Semantic (ExprWithoutBlock 'Checked, Type 'Checked)
exprWithoutBlock = \case
    LitExpr l -> pure (LitExpr l, litType l)
    UncheckedVarExpr x -> do
        t <- findValue x
        pure (CheckedVarExpr x t, t)
    UncheckedConstExpr x -> do
        t <- findConst x
        pure (CheckedConstExpr x t, t)
    UnaryOpExpr o e -> do
        (e', t) <- exprWithoutBlock e
        case (o, t) of
            (Neg, I64) -> pure (UnaryOpExpr Neg e', I64)
            (Neg, F64) -> pure (UnaryOpExpr FNeg e', F64)
            (Not, Bool) -> pure (UnaryOpExpr Not e', Bool)
            (As F64, I64) -> pure (UnaryOpExpr AsF64 e', F64)
            (As I64, F64) -> pure (UnaryOpExpr AsI64 e', I64)
            _ -> throwError (UnaryOpTypeMismatch o e t)
    BinaryOpExpr o e0 e1 -> do
        (e0', t0) <- exprWithoutBlock e0
        (e1', t1) <- exprWithoutBlock e1
        case (o, t0, t1) of
            (Mul, I64, I64) -> pure (BinaryOpExpr Mul e0' e1', I64)
            (Mul, F64, F64) -> pure (BinaryOpExpr FMul e0' e1', F64)
            (Div, I64, I64) -> pure (BinaryOpExpr Div e0' e1', I64)
            (Div, F64, F64) -> pure (BinaryOpExpr FDiv e0' e1', F64)
            (Rem, I64, I64) -> pure (BinaryOpExpr Rem e0' e1', I64)
            (Rem, F64, F64) -> pure (BinaryOpExpr FRem e0' e1', F64)
            (Add, I64, I64) -> pure (BinaryOpExpr Add e0' e1', I64)
            (Add, F64, F64) -> pure (BinaryOpExpr FAdd e0' e1', F64)
            (Sub, I64, I64) -> pure (BinaryOpExpr Sub e0' e1', I64)
            (Sub, F64, F64) -> pure (BinaryOpExpr FSub e0' e1', F64)
            (Shl, I64, I64) -> pure (BinaryOpExpr Shl e0' e1', I64)
            (Shr, I64, I64) -> pure (BinaryOpExpr Shr e0' e1', I64)
            (BitAnd, I64, I64) -> pure (BinaryOpExpr BitAnd e0' e1', I64)
            (BitXor, I64, I64) -> pure (BinaryOpExpr BitXor e0' e1', I64)
            (BitOr, I64, I64) -> pure (BinaryOpExpr BitOr e0' e1', I64)
            (Cmp p, I64, I64) -> pure (BinaryOpExpr (Icmp p) e0' e1', Bool)
            (Cmp p, F64, F64) -> pure (BinaryOpExpr (Fcmp p) e0' e1', Bool)
            (And, Bool, Bool) -> pure (BinaryOpExpr And e0' e1', Bool)
            (Or, Bool, Bool) -> pure (BinaryOpExpr Or e0' e1', Bool)
            _ -> throwError (BinaryOpTypeMismatch o e0 t0 e1 t1)
    UncheckedCallExpr n es -> do
        ets' <- traverse exprWithoutBlock es
        let ts = fmap snd ets'
        (argumentTypes, returnType) <- findFn n
        if ts == argumentTypes then
            pure (CheckedCallExpr n (fmap fst ets') returnType, returnType)
        else
            throwError (CallExprTypeMismatch n es ts)
    UncheckedStructExpr n es -> do
        ets' <- traverse exprWithoutBlock es
        let ts = fmap snd ets'
        fieldTypes <- findStruct n
        if ts == fieldTypes then
            pure (CheckedStructExpr n ets', Struct n)
        else
            throwError (StructExprTypeMismatch n es ts)
    UncheckedEnumExpr n v es -> do
        ets' <- traverse exprWithoutBlock es
        let ts = fmap snd ets'
        (i, fieldTypes) <- findEnumVariant n v
        if ts == fieldTypes then
            pure (CheckedEnumExpr n i ets', Enum n)
        else
            throwError (EnumExprTypeMismatch n v es ts)
    PrintLnExpr (UncheckedFormatString cs) es -> do
        ets' <- traverse exprWithoutBlock es

        let ts = fmap snd ets'

        builder <- loop mempty cs ts

        let f = Text.Lazy.toStrict (Text.Lazy.Builder.toLazyText (builder <> "\n"))

        i <- bindString f

        let es' = fmap fst ets'

        pure (PrintLnExpr (CheckedFormatString i) es', Unit)
      where
        loop builder chunks types =
            let (builder', chunks') = nonHoles builder chunks in

            hole builder' chunks' types

        nonHoles builder chunks =
            let (prefix, chunks') = span (/= Hole) chunks in

            let builder' = builder <> mconcat [Text.Lazy.Builder.fromText x | NonHole x <- prefix] in

            (builder', chunks')

        hole builder chunks types =
            case (chunks, types) of
                -- There are neither holes nor arguments left, so we are done.
                ([], []) -> pure builder
                -- There are too many holes.
                (c : _, []) -> throwError (PrintLnExprExcessHole c)
                -- There are too many arguments.
                ([], t : _) -> throwError (PrintLnExprExcessArg t)
                -- There are both holes and arguments left.
                (Hole : chunks', t : types') -> do
                    s <- formatSpecifier t
                    loop (builder <> s) chunks' types'
                -- Unreachable.
                (c : _, _) -> error ("Internal error: unexpected `hole _ (" <> show c <> " : _) _`.")

        formatSpecifier = \case
            I64 -> pure "%lld"
            F64 -> pure "%f"
            t -> throwError (PrintLnExprTypeMismatch t)

constDef
    :: ConstDecl 'Checked
    -> ConstInit 'Unchecked
    -> Semantic (ConstDef 'Checked)
constDef d@(ConstDecl _ constDeclType) c = do
    (c', t) <- constInit c
    if constDeclType == t then
        pure (ConstDef d c')
    else
        throwError (ConstDefTypeMismatch d c t)

constInit
    :: ConstInit 'Unchecked
    -> Semantic (ConstInit 'Checked, Type 'Checked)
constInit = \case
    LitInit l -> pure (LitInit l, litType l)
    UncheckedNegLitInit l ->
        case l of
            I64Lit x -> pure (NegI64LitInit x, I64)
            F64Lit x -> pure (NegF64LitInit x, F64)
            _ -> throwError (NegLitInitTypeMismatch l (litType l))
    StructInit n cs -> do
        fieldTypes <- findStruct n
        cts' <- traverse constInit cs
        let ts = fmap snd cts'
        if ts == fieldTypes then
            pure (StructInit n (fmap fst cts'), Struct n)
        else
            throwError (StructInitTypeMismatch n cs ts)
    UncheckedEnumInit n v cs -> do
        (i, fieldTypes) <- findEnumVariant n v
        cts' <- traverse constInit cs
        let ts = fmap snd cts'
        if ts == fieldTypes then
            pure (CheckedEnumInit n i (fmap fst cts'), Enum n)
        else
            throwError (EnumInitTypeMismatch n v cs ts)
