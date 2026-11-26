-- | Helpers for building up UPLC @Term@s.
--
-- @since 2.0.0
module Plutarch.Backend.UPLC
  ( UPLCTerm (..),
    eraseUPLC,
    fromRawUPLC,
    uplcApply,
    uplcPackedApply,
    uplcApply1,
    uplcLam,
    uplcLam1,
    uplcLet,
    uplcLetP,
    uplcLetLazy,
    uplcLetPLazy,
    uplcForce,
    uplcDelay,
    uplcVar,
    uplcConstant,
    uplcBuiltin,
    uplcError,
    uplcConstr,
    uplcCase,
    uplcMCombinator,
    rewriteUniques,
    substituteVarFor,
    optimizeUplc,
  )
where

import Control.Monad (void)
import Control.Monad.State.Strict
  ( MonadState,
    gets,
    modify,
    runState,
  )
import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.Default (def)
import Data.Hashable (Hashable, hash)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Data.Word (Word64)
import Plutarch.Helpers.Pretty (UPLCForm (..), prettyUPLC)
import PlutusCore (Some, ValueOf)
import PlutusCore qualified as PLC
import Prettyprinter (Pretty (pretty))
import UntypedPlutusCore qualified as UPLC

-- | A wrapper around the polymorphism soup that is the UPLC @Term@. This is
-- both simpler and more concise to work with, and also allows us to provide our
-- own helpers.
--
-- @since 2.0.0
newtype UPLCTerm = UPLCTerm (UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun UPLCForm)
  deriving stock
    ( -- | @since 2.0.0
      Show
    )
  deriving
    ( -- | @since 2.0.0
      Eq,
      -- | @since 2.0.0
      Hashable
    )
    via (UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun UPLCForm)

-- | Erase display metadata for serialization or comparison without display tags.
eraseUPLC :: UPLCTerm -> UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun ()
eraseUPLC (UPLCTerm t) = void t

-- | Import raw UPLC, marking leading bindings for display.
fromRawUPLC :: UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun () -> UPLCTerm
fromRawUPLC t = UPLCTerm (markLeadingLets (Plain <$ t))

-- | Mark bindings along the outer function body without changing their right-hand sides.
markLeadingLets ::
  forall (name :: Type) (uni :: Type -> Type) (fun :: Type).
  UPLC.Term name uni fun UPLCForm ->
  UPLC.Term name uni fun UPLCForm
markLeadingLets = \case
  UPLC.LamAbs ann name body -> UPLC.LamAbs ann name (markLeadingLets body)
  UPLC.Apply _ (UPLC.LamAbs ann name body) rhs ->
    UPLC.Apply AsLeadingLet (UPLC.LamAbs ann name (markLeadingLets body)) rhs
  other -> other

-- | @since 2.0.0
instance Pretty UPLCTerm where
  pretty (UPLCTerm t) = prettyUPLC t

-- | The @M@ combinator, or @\x -> x@. The argument provides the name of its
-- variable (in this case, @x@).
--
-- @since 2.0.0
uplcMCombinator :: UPLC.Name -> UPLCTerm
uplcMCombinator arg = uplcLam1 arg . uplcApply1 (uplcVar arg) . uplcVar $ arg

-- | @'uplcLet' name v f@ produces the equivalent of @(\name -> f) v@.
--
-- @since 2.0.0
uplcLet :: UPLC.Name -> UPLCTerm -> UPLCTerm -> UPLCTerm
uplcLet varName (UPLCTerm v) (UPLCTerm f) =
  UPLCTerm (UPLC.Apply AsLet (UPLC.LamAbs Plain varName f) v)

-- | As 'uplcLet', but wraps the binding in @delay@.
--
-- @since 2.0.0
uplcLetLazy :: UPLC.Name -> UPLCTerm -> UPLCTerm -> UPLCTerm
uplcLetLazy varName (UPLCTerm v) (UPLCTerm f) =
  UPLCTerm (UPLC.Apply AsLet (UPLC.LamAbs Plain varName f) (UPLC.Delay Plain v))

-- | \'Parallel\' @let@, as a combination of @constr@ and @case@.
--
-- @since 2.0.0
uplcLetP :: NonEmptyVector (UPLC.Name, UPLCTerm) -> UPLCTerm -> UPLCTerm
uplcLetP nameBodies f =
  let scrut = uplcConstr 0 (NEVector.toVector . fmap snd $ nameBodies)
   in uplcCaseWith AsParallelLet scrut . NEVector.singleton . uplcLam (fmap fst nameBodies) $ f

-- | As 'uplcLetP', but wraps all bindings in @delay@.
uplcLetPLazy :: NonEmptyVector (UPLC.Name, UPLCTerm) -> UPLCTerm -> UPLCTerm
uplcLetPLazy nameBodies f =
  let scrut = uplcConstr 0 (NEVector.toVector . fmap (UPLCTerm . UPLC.Delay Plain . coerce . snd) $ nameBodies)
   in uplcCaseWith AsParallelLet scrut . NEVector.singleton . uplcLam (fmap fst nameBodies) $ f

-- | Arity-1 application.
--
-- @since 2.0.0
uplcApply1 :: UPLCTerm -> UPLCTerm -> UPLCTerm
uplcApply1 (UPLCTerm f) (UPLCTerm x) = UPLCTerm . UPLC.Apply AsApplication f $ x

-- | Arbitrary-arity application.
--
-- @since 2.0.0
uplcApply :: UPLCTerm -> NonEmptyVector UPLCTerm -> UPLCTerm
uplcApply = NEVector.foldl' uplcApply1

-- | Application encoded as a case over packed arguments.
uplcPackedApply :: UPLCTerm -> NonEmptyVector UPLCTerm -> UPLCTerm
uplcPackedApply f args =
  uplcCaseWith AsApplication (uplcConstr 0 (NEVector.toVector args)) (NEVector.singleton f)

-- | The @force@ from UPLC.
--
-- @since 2.0.0
uplcForce :: UPLCTerm -> UPLCTerm
uplcForce (UPLCTerm x) = UPLCTerm . UPLC.Force Plain $ x

-- | The @delay@ from UPLC.
--
-- @since 2.0.0
uplcDelay :: UPLCTerm -> UPLCTerm
uplcDelay (UPLCTerm x) = UPLCTerm . UPLC.Delay Plain $ x

-- | A variable with the given name.
--
-- @since 2.0.0
uplcVar :: UPLC.Name -> UPLCTerm
uplcVar = UPLCTerm . UPLC.Var Plain

-- | The given constant.
--
-- @since 2.0.0
uplcConstant :: Some (ValueOf UPLC.DefaultUni) -> UPLCTerm
uplcConstant = UPLCTerm . UPLC.Constant Plain

-- | The given builtin.
--
-- @since 2.0.0
uplcBuiltin :: UPLC.DefaultFun -> UPLCTerm
uplcBuiltin = UPLCTerm . UPLC.Builtin Plain

-- | The canonical error term.
--
-- @since 2.0.0
uplcError :: UPLCTerm
uplcError = UPLCTerm . UPLC.Error $ Plain

-- | A UPLC @constr@.
--
-- @since 2.0.0
uplcConstr :: Word64 -> Vector UPLCTerm -> UPLCTerm
uplcConstr tag = UPLCTerm . UPLC.Constr Plain tag . Vector.toList . fmap coerce

-- | A UPLC @case@.
--
-- @since 2.0.0
uplcCase :: UPLCTerm -> NonEmptyVector UPLCTerm -> UPLCTerm
uplcCase = uplcCaseWith AsMatch

-- | Build a case with a display form for the pretty-printer.
uplcCaseWith :: UPLCForm -> UPLCTerm -> NonEmptyVector UPLCTerm -> UPLCTerm
uplcCaseWith form (UPLCTerm scrut) = UPLCTerm . UPLC.Case form scrut . NEVector.toVector . fmap coerce

-- | A lambda of arbitrary arity.
--
-- @since 2.0.0
uplcLam :: NonEmptyVector UPLC.Name -> UPLCTerm -> UPLCTerm
uplcLam argNames body = NEVector.foldr uplcLam1 body argNames

-- | A lambda of specifically arity 1.
--
-- @since 2.0.0
uplcLam1 :: UPLC.Name -> UPLCTerm -> UPLCTerm
uplcLam1 varName (UPLCTerm body) = UPLCTerm . UPLC.LamAbs Plain varName $ body

-- | @since 2.0.0
rewriteUniques :: UPLCTerm -> Set Int -> (UPLCTerm, Set Int)
rewriteUniques (UPLCTerm code) used = case runState (go code) (used, Map.empty) of
  (code', (used', _)) -> (UPLCTerm code', used')
  where
    go ::
      forall (m :: Type -> Type).
      (MonadState (Set Int, Map Int Int) m) =>
      UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun UPLCForm ->
      m (UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun UPLCForm)
    go = \case
      t@(UPLC.Var ann (UPLC.Name name (UPLC.Unique uniq))) -> do
        -- Should we rewrite this?
        mRewrite <- gets (Map.lookup uniq . snd)
        case mRewrite of
          Just uniq' -> pure . UPLC.Var ann . UPLC.Name name . UPLC.Unique $ uniq'
          -- Does this clash?
          Nothing -> do
            clashes <- gets (Set.member uniq . fst)
            if clashes
              then do
                -- Rehash the current 'unique' until it no longer clashes
                uniq' <- doUntilM uniq (\x -> pure . hash $ (1 :: Int, x)) (\hash' -> gets (Set.notMember hash' . fst))
                -- Store the rewrite, and note that the new unique is used
                modify (bimap (Set.insert uniq') (Map.insert uniq uniq'))
                -- Apply the rewrite
                pure . UPLC.Var ann . UPLC.Name name . UPLC.Unique $ uniq'
              else pure t
      UPLC.LamAbs ann allName@(UPLC.Name name (UPLC.Unique uniq)) body -> do
        body' <- go body
        -- Should we rewrite our bound var?
        mRewrite <- gets (Map.lookup uniq . snd)
        pure $ case mRewrite of
          Just uniq' -> UPLC.LamAbs ann (UPLC.Name name . UPLC.Unique $ uniq') body'
          Nothing -> UPLC.LamAbs ann allName body'
      UPLC.Apply ann f x -> UPLC.Apply ann <$> go f <*> go x
      UPLC.Force ann body -> UPLC.Force ann <$> go body
      UPLC.Delay ann body -> UPLC.Delay ann <$> go body
      UPLC.Constant ann c -> pure . UPLC.Constant ann $ c
      UPLC.Builtin ann f -> pure . UPLC.Builtin ann $ f
      UPLC.Error ann -> pure . UPLC.Error $ ann
      UPLC.Constr ann tag fields -> UPLC.Constr ann tag <$> traverse go fields
      UPLC.Case ann scrut handlers -> UPLC.Case ann <$> go scrut <*> traverse go handlers

-- | @since 2.0.0
substituteVarFor :: UPLC.Name -> UPLCTerm -> UPLCTerm -> UPLCTerm
substituteVarFor name (UPLCTerm toSub) (UPLCTerm t) = UPLCTerm . go $ t
  where
    go ::
      UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun UPLCForm ->
      UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun UPLCForm
    go = \case
      t'@(UPLC.Var _ name') ->
        if name' == name
          then toSub
          else t'
      UPLC.LamAbs ann name' body -> UPLC.LamAbs ann name' (go body)
      UPLC.Apply ann f x -> UPLC.Apply ann (go f) (go x)
      UPLC.Force ann body -> UPLC.Force ann (go body)
      UPLC.Delay ann body -> UPLC.Delay ann (go body)
      t'@(UPLC.Constant _ _) -> t'
      t'@(UPLC.Builtin _ _) -> t'
      t'@(UPLC.Error _) -> t'
      UPLC.Constr ann tag fields -> UPLC.Constr ann tag (fmap go fields)
      UPLC.Case ann scrut handlers -> UPLC.Case ann (go scrut) (fmap go handlers)

-- | @since 2.0.0
optimizeUplc :: UPLCTerm -> Either UPLC.FreeVariableError UPLCTerm
optimizeUplc (UPLCTerm t) = case PLC.runQuoteT . UPLC.optimizeTerm optimizeOpts def $ t of
  -- Note (Koz, 05/10/2026): For some ungodly reason, only an explicit pattern
  -- match works here. Probably something overly-polymorphic on Plutus' end.
  Left err -> Left err
  Right optimized -> Right . UPLCTerm . markLeadingLets $ optimized
  where
    -- Note (Koz, 05/10/2026): The full list is in
    -- UntypedPlutusCore.Optimize.Opts. The defaults are fine, but re-inlining
    -- of constants blows out code size by no small margin, so we disable it.
    -- Note (Sean, 10/20/2026): ooInlineUnconditionalGrowth seems to fix this but
    -- given the sparsity of docs and complexity of the implementation it isn't obviously clear how.
    -- The only other relevant option would be ooInlineCallsiteGrowth, but low values of that don't fix this
    -- and only modify other goldens.
    optimizeOpts :: UPLC.OptimizeOpts UPLC.Name UPLCForm
    optimizeOpts =
      UPLC.defaultOptimizeOpts
        { UPLC._ooInlineConstants = False,
          UPLC._ooInlineUnconditionalGrowth = 0
        }

-- Helpers

doUntilM ::
  forall (a :: Type) (m :: Type -> Type).
  (Monad m) =>
  a ->
  (a -> m a) ->
  (a -> m Bool) ->
  m a
doUntilM x act cond = do
  res <- act x
  stop <- cond res
  if stop then pure res else doUntilM res act cond
