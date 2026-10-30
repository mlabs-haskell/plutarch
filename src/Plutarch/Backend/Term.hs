{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE ViewPatterns #-}

-- | The full definition of Plutarch 'Term's, as well as eDSL primitives which
-- everything else is built from.
--
-- This design follows the description of e-summaries in /Hashing Modulo
-- Alpha-Equivalence/. However, we also work in a monadic stack providing access
-- to some configuration information, as well as a source of unique identifiers
-- to ensure that the Barendregt convention is followed by any code generated in
-- Plutarch. Lastly, we provide an \'indicator\' to ensure that we never attempt
-- to compile terms that aren't closed, using something akin to the \'@ST@
-- trick\'.
--
-- = Links
--
-- - [The original paper](https://arxiv.org/pdf/2105.02856)
-- - [_Lazy Functional State
-- Threads_](https://www.microsoft.com/en-us/research/wp-content/uploads/1994/06/lazy-functional-state-threads.pdf),
-- which describes the \'@ST@ trick\'
--
-- @since 2.0.0
module Plutarch.Backend.Term
  ( TracingMode (..),
    OptimizationMode (..),
    TermEnv (..),
    debugTermEnv,
    releaseTermEnv,
    S,
    Term (..),
    SomeTerm (..),
    toSomeTerm,
    TermError (..),
    PDelayed,
  )
where

import Control.Monad (join)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.RWS.CPS
  ( MonadReader (..),
    RWS,
    gets,
    modify',
    runRWS,
  )
import Data.Kind (Type)
import Data.List (find)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.These.Combinators (justHere, justThere)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Data.Word (Word64)
import GHC.Stack (CallStack)
import Plutarch.Backend.PosTree
  ( PosTree
      ( PCase,
        PCompose,
        PHere,
        PMany,
        POne,
        PTwo
      ),
  )
import Plutarch.Backend.RawTerm
  ( RawTerm
      ( RApply,
        RBuiltin,
        RCase,
        RCompiled,
        RCompose,
        RConstant,
        RConstr,
        RDelay,
        RError,
        RFix,
        RForce,
        RLamAbs,
        RPlaceholder,
        RVar
      ),
    VarTag (Argument, Self),
  )
import Plutarch.Backend.VarMap (VarMap, vmFold)
import Plutarch.Helpers.Pretty
  ( PrintMode (PrintAtomic, PrintDefault),
    appTemplate,
    blockParens,
    caseTemplate,
    compactReadableVar,
    composeTemplate,
    ctorTemplate,
    lambdaTemplate,
    prettyValueOf,
  )
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    align,
    angles,
    braces,
    brackets,
    group,
    viaShow,
    (<+>),
  )
import Universe (Some (Some), ValueOf (ValueOf))

-- Note (Koz, 08/06/2026): `type data` is a particularly hideous GHCism used to
-- define (effectively) a kind without forcing us to promote a type. Whenever
-- you see `type data Foo = Bar | Baz`, you can read that as a declaration of a
-- new _kind_ `Foo`, containing the types `Bar` and `Baz`.

-- | An empty kind to ensure that the @s@ parameter never gets used for
-- anything.
--
-- @since 2.0.0
type data S

-- | What level of tracing to use in a compiled result.
--
-- @since 2.0.0
data TracingMode
  = NoTracing
  | ErrorTracing
  | DebugTracing
  deriving stock
    ( -- | @since 2.0.0
      Eq,
      -- | @since 2.0.0
      Show
    )

-- | Whether to use external optimizations on the generated code or not.
--
-- Currently, \'external optimizations\' consist of the following:
--
-- * Pre-evaluation of the generated code; followed by
-- * The UPLC optimizer provided in Plutus Core.
--
-- These are labelled as \'external\' as Plutarch does not control them.
--
-- @since 2.0.0
data OptimizationMode = OnlyInternal | InternalExternal
  deriving stock
    ( -- | @since 2.0.0
      Eq,
      -- | @since 2.0.0
      Show
    )

-- | A configuration environment for 'Term's and their compilation.
--
-- @since 2.0.0
data TermEnv = TermEnv TracingMode OptimizationMode
  deriving stock
    ( -- | @since 2.0.0
      Eq,
      -- | @since 2.0.0
      Show
    )

-- | An environment suitable for debugging. This currently means the following:
--
-- * Tracing enabled at the most verbose level ('DebugTracing'); and
-- * External optimizations disabled ('OnlyInternal).
--
-- @since 2.0.0
debugTermEnv :: TermEnv
debugTermEnv = TermEnv DebugTracing OnlyInternal

-- | An environment suitable for final production. This currently means the
-- following:
--
-- * Tracing turned off ('NoTracing'); and
-- * External optimizations enabled ('InternalExternal').
--
-- @since 2.0.0
releaseTermEnv :: TermEnv
releaseTermEnv = TermEnv NoTracing InternalExternal

-- | Various errors that can arise during 'Term' construction.
--
-- @since 2.0.0
data TermError
  = -- | A functional's @self@ argument went unused. This means no fixed point
    --    will be found, and is definitely a mistake.
    --
    --    @since 2.0.0
    UnusedSelfArgument
  | -- | A general space for any user-specific errors.
    --
    --    @since 2.0.0
    UserSpecified CallStack Text
  | -- | An @apply@ construction caused a bad position tree merge. This is
    --    something that should not happen normally, and is /definitely/ a bug!
    --
    --    @since 2.0.0
    BadMergeApply Word64 PosTree PosTree
  | -- | An @apply@ or @case@ construction caused a bad position tree merge.
    --    This is something that should not happen normally, and is /definitely/
    --    a bug!
    --
    --    @since 2.0.0
    BadMergeCase Word64 PosTree PosTree
  | -- | A @constr@ construction caused a bad position tree merge. This is
    --    something that should not happen normally, and is /definitely/ a bug!
    --
    --    @since 2.0.0
    BadMergeConstr Word64 PosTree PosTree
  | -- | A composition construction caused a bad position tree merge. This
    --    is something that should not happen normally, and is /definitely/ a
    --    bug!
    --
    --    @since 2.0.0
    BadMergeCompose Word64 PosTree PosTree
  deriving stock
    ( -- | @since 2.0.0
      Show
    )

-- | A basic unit of Plutarch eDSL construction. More precisely, a @'Term' s a@
-- is the code which corresponds to a computation that, when run, will either
-- produce the UPLC equivalent of @a@, or error. The @s@ is used to track free
-- variable dependencies, specifically to ensure that we never attempt to
-- compile a 'Term' with free variables.
--
-- @since 2.0.0
newtype Term (s :: S) (a :: S -> Type)
  = Term {asRawTerm :: ExceptT TermError (RWS TermEnv () Word64) (VarMap, RawTerm ())}

type role Term nominal representational

instance Pretty (Term s a) where
  pretty = prettyTerm

prettyTerm :: forall (s :: S) (px :: S -> Type) (ann :: Type). Term s px -> Doc ann
prettyTerm t = case runRWS (runExceptT (asRawTerm t)) debugTermEnv 0 of
  (res, _, _) -> case res of
    Left e -> "Encountered an error when attempting to pretty print a term: " <> pretty (show e)
    Right (varMap, term) -> case runRWS (withVarMap varMap $ go term) mempty 0 of
      (res', _, _) -> res'
  where
    mapMaybeSet :: forall (a :: Type). (Ord a) => (a -> Maybe a) -> Set a -> Set a
    mapMaybeSet f = Set.foldl' (\acc x -> case f x of Just y -> Set.insert y acc; _ -> acc) mempty

    withVarMap ::
      forall (r :: Type).
      VarMap ->
      RWS (Set (PosTree, Text)) () Integer r ->
      RWS (Set (PosTree, Text)) () Integer r
    withVarMap vm act = do
      let posTrees = vmFold (\acc _ new -> Vector.snoc acc new) mempty vm
      namedTrees <- Vector.foldM (\acc pt -> nextVarName Nothing >>= \nm -> pure $ Set.insert (pt, nm) acc) mempty posTrees
      local (namedTrees <>) act

    -- "Nothing" means "Free" (since we don't have a VarTag ctor for it)
    nextVarName :: Maybe VarTag -> RWS (Set (PosTree, Text)) () Integer Text
    nextVarName vt = do
      s <- gets compactReadableVar
      modify' (+ 1)
      let suffix = case vt of
            Nothing -> "_free"
            Just tag -> case tag of
              Argument -> ""
              Self -> "_self"
      pure (s <> suffix)

    withBoundVar ::
      forall (a :: Type).
      (VarTag, PosTree) ->
      (PosTree -> Maybe PosTree) ->
      RWS (Set (PosTree, Text)) () Integer a ->
      RWS (Set (PosTree, Text)) () Integer (Doc ann, a)
    withBoundVar (vt, pt) localF cont = do
      varName <- nextVarName (Just vt)
      let pVar = pretty varName
      (pVar,) <$> local (Set.insert (pt, varName) . mapMaybe1 localF) cont

    resolveThisVar :: RWS (Set (PosTree, Text)) () Integer (Doc ann)
    resolveThisVar = do
      pts <- ask
      case snd <$> find (\x -> fst x == PHere) pts of
        Nothing -> error "free variable in Term, this should be impossible (also: Implement better error handling in prettyTerm)"
        Just thisVar -> pure (pretty thisVar)

    mapMaybe1 :: forall (c :: Type) (d :: Type). (Ord c, Ord d) => (c -> Maybe c) -> Set (c, d) -> Set (c, d)
    mapMaybe1 f = mapMaybeSet (\(x, y) -> (,) <$> f x <*> pure y)

    downL :: PosTree -> Maybe PosTree
    downL = \case
      PTwo xs -> justHere xs
      _ -> Nothing

    downR :: PosTree -> Maybe PosTree
    downR = \case
      PTwo xs -> justThere xs
      _ -> Nothing

    downOne :: PosTree -> Maybe PosTree
    downOne = \case (POne x) -> Just x; _ -> Nothing

    downScrut :: PosTree -> Maybe PosTree
    downScrut = \case (PCase pscrut _) -> pscrut; _ -> Nothing

    downHandler :: Int -> PosTree -> Maybe PosTree
    downHandler n = \case
      PCase _ handlers -> join $ handlers NEVector.!? n
      _ -> Nothing

    downMany :: Int -> PosTree -> Maybe PosTree
    downMany n = \case
      PMany xs -> join $ xs Vector.!? n
      _ -> Nothing

    downCompose :: Int -> PosTree -> Maybe PosTree
    downCompose n = \case
      PCompose xs -> join $ xs NEVector.!? n
      _ -> Nothing

    isSmall :: RawTerm () -> Bool
    isSmall = \case
      RVar {} -> True
      RConstant {} -> True
      RError {} -> True
      RBuiltin {} -> True
      RDelay _ inner -> isSmall inner
      RForce _ inner -> isSmall inner
      _ -> False

    tagArg :: Maybe PosTree -> Maybe (VarTag, PosTree)
    tagArg = fmap (Argument,)

    goLambda ::
      [Doc ann] ->
      Maybe (VarTag, PosTree) ->
      RawTerm () ->
      RWS (Set (PosTree, Text)) () Integer ([Doc ann], Doc ann)
    goLambda acc mpt = \case
      RLamAbs _ (tagArg -> cMPT) cBody -> case mpt of
        Nothing -> do
          (innerVars, innerBody) <- local (mapMaybe1 downOne) $ goLambda acc cMPT cBody
          pure ("_" : innerVars, innerBody)
        Just pt -> do
          (thisVar, (thoseVars, body)) <- withBoundVar pt downOne $ goLambda acc cMPT cBody
          pure (thisVar : thoseVars, body)
      otherBody -> case mpt of
        Nothing -> do
          innerBody <- local (mapMaybe1 downOne) $ go otherBody
          pure (reverse ("_" : acc), innerBody)
        Just pt -> do
          (thisVar, body) <- withBoundVar pt downOne $ go otherBody
          pure (reverse (thisVar : acc), body)

    goApp :: RawTerm () -> RWS (Set (PosTree, Text)) () Integer (NonEmptyVector (Doc ann))
    goApp = \case
      RApply _ f arg -> do
        xs <- local (mapMaybe1 downL) $ goApp f
        parg <- local (mapMaybe1 downR) $ goAtomic arg
        pure $ xs <> NEVector.singleton parg
      other -> do
        res <- goAtomic other
        pure . NEVector.singleton $ res

    go :: RawTerm () -> RWS (Set (PosTree, Text)) () Integer (Doc ann)
    go = go' PrintDefault

    goAtomic :: RawTerm () -> RWS (Set (PosTree, Text)) () Integer (Doc ann)
    goAtomic = go' PrintAtomic

    go' :: PrintMode -> RawTerm () -> RWS (Set (PosTree, Text)) () Integer (Doc ann)
    go' mode = \case
      RVar _ _ -> resolveThisVar
      RLamAbs _ mpt body -> do
        (pvars, body') <- goLambda [] (tagArg mpt) body
        pure $ lambdaTemplate mode pvars body'
      RForce _ inner -> do
        pinner <- local (mapMaybe1 downOne) $ goAtomic inner
        pure $ "!" <> pinner
      RDelay _ inner -> do
        pinner <- local (mapMaybe1 downOne) $ goAtomic inner
        pure $ angles pinner
      app@(RApply {}) -> do
        pfunList <- goApp app
        pure $ appTemplate mode pfunList
      RConstant _ (Some (ValueOf uni x)) ->
        pure $ prettyValueOf uni x
      RBuiltin _ bif ->
        pure $ viaShow bif
      RCompiled _ uplc ->
        pure . align . group $ "COMPILED" <> braces (pretty uplc)
      RError _ ->
        pure "ERROR"
      RPlaceholder _ i ->
        pure $ "$PLACEHOLDER" <> brackets (pretty i)
      RCase _ scrut handlers -> do
        pscrut <- local (mapMaybe1 downScrut) $ goAtomic scrut
        phandlers <- NEVector.toList <$> NEVector.imapM (\i -> local (mapMaybe1 (downHandler i)) . go) handlers
        pure $ caseTemplate mode (isSmall scrut) pscrut phandlers
      RConstr _ cix args -> do
        pargs <- Vector.toList <$> Vector.imapM (\i arg -> local (mapMaybe1 (downMany i)) $ go arg) args
        pure $ ctorTemplate mode cix pargs
      RCompose _ components -> do
        pcomponents <- NEVector.imapM (\i -> local (mapMaybe1 (downCompose i)) . goAtomic) components
        pure $ composeTemplate mode pcomponents
      RFix _ pt body -> do
        (pvars, pbody) <- goLambda [] (Just (Self, pt)) body
        let resNoParens = "fix" <+> align (lambdaTemplate PrintAtomic pvars pbody)
        case mode of
          PrintDefault -> pure resNoParens
          PrintAtomic -> pure $ blockParens resNoParens

-- | A 'Term' whose result has been forgotten. Useful mainly together with
-- 'punsafeCase' and 'punsafeConstr', as it allows fields and handlers of
-- heterogenous types to be used in both.
--
-- @since 2.0.0
data SomeTerm (s :: S) where
  SomeTerm :: Term s a -> SomeTerm s

-- | \'Forgets\' the result of a computation represented by a 'Term'.
--
-- @since 2.0.0
toSomeTerm ::
  forall (a :: S -> Type) (s :: S).
  Term s a -> SomeTerm s
toSomeTerm = SomeTerm

-- | A type-level tag indicating a Plutarch computation that has been
-- \'suspended\'. This tag can be removed by using 'pforce', and added by using
-- 'pdelay'.
--
-- @since 2.0.0
data PDelayed (a :: S -> Type) (s :: S)
