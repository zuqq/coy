{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coy.Semantic
    (
    -- * Error type
      SemanticError (..)
    , SemanticErrorMessage (..)
    -- * Entry point
    , semantic
    )
    where

import Algebra.Graph (stars)
import Algebra.Graph.ToGraph (topSort)
import Control.Monad (foldM, when, zipWithM)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Trans.Except (Except, runExcept)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Data.Bifunctor (first)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList, traverse_)
import Data.List (partition, sort, sortOn)
import Data.List.Index (indexed)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Traversable (for)
import Data.Vector (Vector)
import Lens.Micro (Lens', _1, lens )
import Lens.Micro.Mtl ((%=), (.=), use, view)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Lazy.Builder
import qualified Data.Vector as Vector

import Coy.Syntax

data Context = Context
    { _structs :: Map Text (Vector (Type 'Checked))
    , _enums :: Map Text (Map Text (Int, Vector (Type 'Checked)))
    , _fns :: Map Text (Vector (Type 'Checked), Type 'Checked)
    , _values :: Map Text (Type 'Checked)
    }
    deriving Show

structs :: Lens' Context (Map Text (Vector (Type 'Checked)))
structs = lens _structs (\c ss -> c {_structs = ss})

enums :: Lens' Context (Map Text (Map Text (Int, Vector (Type 'Checked))))
enums = lens _enums (\c es -> c {_enums = es})

fns :: Lens' Context (Map Text (Vector (Type 'Checked), Type 'Checked))
fns = lens _fns (\c fs -> c {_fns = fs})

values :: Lens' Context (Map Text (Type 'Checked))
values = lens _values (\c vs -> c {_values = vs})

instance Semigroup Context where
    Context ss0 es0 fs0 vs0 <> Context ss1 es1 fs1 vs1 =
        Context (ss0 <> ss1) (es0 <> es1) (fs0 <> fs1) (vs0 <> vs1)

instance Monoid Context where
    mempty = Context mempty mempty mempty mempty

data SemanticErrorMessage
    = RedefinedTypes [TypeDef 'Unchecked]
    | RedefinedFns [FnDecl 'Unchecked]
    | StructOrEnumNotFound Text
    | TypeCycle (NonEmpty (TypeDef 'Unchecked))
    | StructNotFound Text
    | EnumNotFound Text
    | EnumVariantNotFound Text Text
    | FnNotFound Text
    | ValueNotFound Text
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
        -- ^ Names of the struct.
        (Vector (ExprWithoutBlock 'Unchecked))
        -- ^ Arguments.
        (Vector (Type 'Checked))
        -- ^ Types of the arguments.
    | EnumExprTypeMismatch
    -- ^ The given argument list doesn't conform to the constructor's signature.
        Text
        -- ^ Name of the enum.
        (EnumVariantAccessor 'Unchecked)
        -- ^ Name of the enum variant.
        (Vector (ExprWithoutBlock 'Unchecked))
        -- ^ Arguments.
        (Vector (Type 'Checked))
        -- ^ Types of the arguments.
    | PrintLnExprTypeMismatch
    -- ^ The argument list contains non-printable expressions or doesn't match
    -- the shape of the format string.
        (FormatString 'Unchecked)
        -- ^ The format string.
        [ExprWithoutBlock 'Unchecked]
        -- ^ Arguments.
        [Type 'Checked]
        -- ^ Types of the arguments.
    deriving Show

data SemanticError = SemanticError Context SemanticErrorMessage
    deriving Show

type Semantic = StateT Context (Except SemanticError)

throwEmptySemanticError :: SemanticErrorMessage -> Either SemanticError a
throwEmptySemanticError e = throwError (SemanticError mempty e)

throwSemanticError :: SemanticErrorMessage -> Semantic a
throwSemanticError e = do
    c <- use id
    throwError (SemanticError c e)

orFail :: Maybe a -> SemanticErrorMessage -> Semantic a
orFail x e = maybe (throwSemanticError e) pure x

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

findFn :: Text -> Semantic (Vector (Type 'Checked), Type 'Checked)
findFn n = do
    fs <- use fns
    Map.lookup n fs `orFail` FnNotFound n

findValue :: Text -> Semantic (Type 'Checked)
findValue n = do
    vs <- use values
    Map.lookup n vs `orFail` ValueNotFound n

bindValue :: Text -> Type 'Checked -> Semantic ()
bindValue x t = values %= Map.insert x t

namespaced :: Semantic a -> Semantic a
namespaced p = do
    backup <- use values
    result <- p
    values .= backup
    pure result

intrinsicFns :: [(Text, (Vector (Type 'Checked), Type 'Checked))]
intrinsicFns = [("sqrt", (Vector.singleton F64, F64))]

semantic :: Module 'Unchecked -> Either SemanticError (Module 'Checked)
semantic (UncheckedModule typeDefs fnDefs) = do
    (typeDefs', fnDecls') <- semantic0 (typeDefs, [d | FnDef d _ <- fnDefs])

    let ss = Map.fromList [(n, ts) | StructDef n ts <- typeDefs']

    let es = Map.fromList [(n, enumVariants vs) | EnumDef n vs <- typeDefs']

    let fs = Map.fromList (
                intrinsicFns
            <>  [(n, (fmap typeOf as, t)) | FnDecl n as t <- fnDecls'])

    let vs = mempty

    let context = Context ss es fs vs

    fnDefs' <- runExcept (
        evalStateT (zipWithM fnDef fnDecls' [b | FnDef _ b <- fnDefs]) context)

    let (mainFnDefs', otherFnDefs') =
            partition (\fd -> fnDefName fd == "main") fnDefs'

    case mainFnDefs' of
        [mainFnDef'@(FnDef (FnDecl _ as t) _)]
            | Vector.null as, t == Unit ->
                pure (CheckedModule typeDefs' mainFnDef' otherFnDefs')
            | otherwise ->
                throwEmptySemanticError (MainFnDefTypeMismatch mainFnDef')
        _ -> throwEmptySemanticError MainFnDefMissing
  where
    enumVariants vs =
        Map.fromList [(v, (i, ts)) | (i, EnumVariant v ts) <- indexed vs]

    typeOf (FnArg _ at) = at

semantic0
    :: ([TypeDef 'Unchecked], [FnDecl 'Unchecked])
    -> Either SemanticError ([TypeDef 'Checked], [FnDecl 'Checked])
semantic0 (tds, fds) = do
    let structNames = Set.fromList [n | StructDef n _ <- tds]

    let enumNames = Set.fromList [n | EnumDef n _ <- tds]

    let typeNames = structNames <> enumNames

    let fnNames = Set.fromList [n | FnDecl n _ _ <- fds]

    -- Check that no type was redefined.
    when
        (Set.size typeNames /= length tds)
        (throwEmptySemanticError (RedefinedTypes tds))

    -- Check that no function was redefined.
    when
        (Set.size fnNames /= length fds)
        (throwEmptySemanticError (RedefinedFns fds))

    let findType = \case
            Unit -> pure Unit
            Bool -> pure Bool
            I64 -> pure I64
            F64 -> pure F64
            StructOrEnum n
                | n `Set.member` structNames -> pure (Struct n)
                | n `Set.member` enumNames -> pure (Enum n)
                | otherwise -> throwEmptySemanticError (StructOrEnumNotFound n)

    -- Name resolution for the type definitions.
    tds' <- for tds (\case
        StructDef n ts -> do
            ts' <- traverse findType ts
            pure (StructDef n ts')
        EnumDef n vs -> do
            vs' <- for vs (\(EnumVariant v ts) -> do
                ts' <- traverse findType ts
                pure (EnumVariant v ts'))
            pure (EnumDef n vs'))

    -- Name resolution for the function declarations.
    fds' <- for fds (\(FnDecl n as t) -> do
        as' <- for as (\(FnArg an at) -> do
            at' <- findType at
            pure (FnArg an at'))
        t' <- findType t
        pure (FnDecl n as' t'))

    -- We now check that the type definitions are acyclic, by constructing a
    -- graph with a vertex for every struct or enum and an edge for every
    -- dependency between them.
    let name = \case
            StructDef n _ -> n
            EnumDef n _ -> n

    let number = (Map.fromList [(name td, i) | (i, td) <- indexed tds] Map.!)

    let neighbors = \case
            StructDef _ ts -> [number n | StructOrEnum n <- toList ts]
            EnumDef _ vs ->
                [number n | EnumVariant _ ts <- vs, StructOrEnum n <- toList ts]

    let graph = stars [(number (name td), neighbors td) | td <- tds]

    let labelWithUncheckedTypeDef = (Map.fromList (indexed tds) Map.!)

    let labelWithCheckedTypeDef = (Map.fromList (indexed tds') Map.!)

    case topSort graph of
        Left typeCycle ->
            throwEmptySemanticError
                    (TypeCycle (fmap labelWithUncheckedTypeDef typeCycle))
        Right typeOrder -> pure (fmap labelWithCheckedTypeDef typeOrder, fds')

fnDef :: FnDecl 'Checked -> Block 'Unchecked -> Semantic (FnDef 'Checked)
fnDef d@(FnDecl n as returnType) b = do
    (b', resultType) <- namespaced (do
        traverse_ bindFnArg as
        block b)
    if returnType == resultType then
        pure (FnDef d b')
    else
        throwSemanticError (FnDefTypeMismatch n returnType resultType)
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
    LetStatement (StructPattern n xs) e -> do
        fieldTypes <- findStruct n
        when
            (Vector.length xs /= Vector.length fieldTypes)
            (throwSemanticError (StructPatternArityMismatch n xs))
        (e', t) <- expr e
        when
            (Struct n /= t)
            (throwSemanticError (StructPatternTypeMismatch n e t))
        Vector.zipWithM_ bindValue xs fieldTypes
        pure (LetStatement (StructPattern n xs) e')
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
        when (t /= Bool) (throwSemanticError (IfScrutineeTypeMismatch e t))
        (b0', t0) <- block b0
        (b1', t1) <- block b1
        when (t0 /= t1) (throwSemanticError (IfBlocksTypeMismatch b0 t0 b1 t1))
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
                    (throwSemanticError (MatchArmEnumMismatch n actualEnums))

                -- Check that the enum variant names exactly cover the variants.
                let actualEnumVariants = [v | UncheckedMatchArm _ v _ _ <- as]
                when
                    (sort actualEnumVariants /= Set.toAscList vs)
                    (throwSemanticError
                        (MatchArmEnumVariantMismatch n actualEnumVariants))

                -- Recurse into each match arm.
                iats' <- for as (\(UncheckedMatchArm _ v xs e) -> do
                    (i, fieldTypes) <- findEnumVariant n v
                    if Vector.length xs == Vector.length fieldTypes then
                        namespaced (do
                            Vector.zipWithM_ bindValue xs fieldTypes
                            (e', resultType) <- expr e
                            pure (i, CheckedMatchArm xs e', resultType))
                    else
                        throwSemanticError (MatchArmArityMismatch n v xs))

                -- Check that the types of the match arms are all the same.
                let resultTypes = [resultType | (_, _, resultType) <- iats']
                case nubOrd resultTypes of
                    [resultType] -> do
                        -- Sort the checked match arms by variant index.
                        let as' = [a' | (_, a', _) <- sortOn (view _1) iats']

                        -- Assemble the checked match expression.
                        pure (CheckedMatchExpr e0' n as', resultType)
                    _ ->
                        throwSemanticError
                            (MatchArmTypeMismatch as resultTypes)
            _ -> throwSemanticError (MatchScrutineeTypeMismatch e0 t0)

exprWithoutBlock
    :: ExprWithoutBlock 'Unchecked
    -> Semantic (ExprWithoutBlock 'Checked, Type 'Checked)
exprWithoutBlock = \case
    LitExpr l ->
        case l of
            UnitLit () -> pure (LitExpr (UnitLit ()), Unit)
            BoolLit b -> pure (LitExpr (BoolLit b), Bool)
            I64Lit x -> pure (LitExpr (I64Lit x), I64)
            F64Lit f -> pure (LitExpr (F64Lit f), F64)
    VarExpr x -> do
        t <- findValue x
        pure (VarExpr x, t)
    UnaryOpExpr o e -> do
        (e', t) <- exprWithoutBlock e
        case (o, t) of
            (Neg, I64) -> pure (UnaryOpExpr Neg e', I64)
            (Neg, F64) -> pure (UnaryOpExpr FNeg e', F64)
            (Not, Bool) -> pure (UnaryOpExpr Not e', Bool)
            (As I64, F64) -> pure (UnaryOpExpr AsI64 e', I64)
            _ -> throwSemanticError (UnaryOpTypeMismatch o e t)
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
            _ -> throwSemanticError (BinaryOpTypeMismatch o e0 t0 e1 t1)
    CallExpr (Call n es) -> do
        ets' <- traverse exprWithoutBlock es
        let ts = fmap snd ets'
        (argumentTypes, returnType) <- findFn n
        if ts == argumentTypes then
            pure (CallExpr (Call n (fmap fst ets')), returnType)
        else
            throwSemanticError (CallExprTypeMismatch n es ts)
    StructExpr n es -> do
        ets' <- traverse exprWithoutBlock es
        let ts = fmap snd ets'
        fieldTypes <- findStruct n
        if ts == fieldTypes then
            pure (StructExpr n (fmap fst ets'), Struct n)
        else
            throwSemanticError (StructExprTypeMismatch n es ts)
    EnumExpr n a@(EnumVariantName v) es -> do
        ets' <- traverse exprWithoutBlock es
        let ts = fmap snd ets'
        (i, fieldTypes) <- findEnumVariant n v
        if ts == fieldTypes then
            pure (EnumExpr n (EnumVariantIndex i) (fmap fst ets'), Enum n)
        else
            throwSemanticError (EnumExprTypeMismatch n a es ts)
    PrintLnExpr f@(UncheckedFormatString cs) es -> do
        ets' <- traverse exprWithoutBlock es

        let ts = fmap snd ets'

        -- Work around the monomorphism restriction with an explicit type.
        let abort :: Semantic a
            abort = throwSemanticError (PrintLnExprTypeMismatch f es ts)

        let formatSpecifier = \case
                I64 -> pure "%lld"
                F64 -> pure "%f"
                _ -> abort

        -- Fold over the list of argument types in order to fill in the holes in
        -- the format string. Because holes and non-holes alternate, there is at
        -- most one leading chunk that is not a hole.
        let step (s, "{}" : cs') t = do
                s' <- formatSpecifier t
                pure (s <> s', cs')
            step (s, c : "{}" : cs') t = do
                s' <- formatSpecifier t
                pure (s <> Text.Lazy.Builder.fromText c <> s', cs')
            -- This covers the case where there are not enough holes for the
            -- given argument list.
            step _ _ = abort

        (builder, leftovers) <- foldM step (mempty, cs) ts

        -- Check that there are no parameters left.
        if "{}" `elem` leftovers then
            abort
        else do
            let result =
                    Text.Lazy.toStrict (Text.Lazy.Builder.toLazyText builder)

            -- At this point there should be at most one chunk left; @mconcat@
            -- conveniently handles both cases. We also make sure to add the
            -- trailing line feed character here.
            let f' = CheckedFormatString (result <> mconcat leftovers <> "\n")

            let es' = fmap fst ets'

            pure (PrintLnExpr f' es', Unit)
