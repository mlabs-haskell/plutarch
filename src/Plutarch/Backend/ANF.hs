{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Administrative normal form representation, with alpha-equivalent binds
-- coalesced.
--
-- = Links
--
-- - [Description of administrative normal
-- form](https://en.wikipedia.org/wiki/A-normal_form)
--
-- @since 2.0.0
module Plutarch.Backend.ANF
  ( BoundVar (..),
    Multiplicity (MultOne),
    Leaf (..),
    Ref (..),
    ANFBind (..),
    Id (..),
    ANF (..),
    BindingStatus (..),
    Dependencies (Deps, NoDeps),
    Analysis (..),
    Rank (..),
    fromHashedAST,
    getANFBindAnn,
    analyzeDeps,
    fullPipeline,
  )
where

import Control.Monad.State.Strict
  ( MonadState,
    State,
    gets,
    modify,
    runState,
  )
import Data.Bifunctor (bimap)
import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import Data.Foldable (for_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Monoid (Sum (Sum))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Data.Vector (MVector, Vector)
import Data.Vector qualified as Vector
import Data.Vector.Mutable (PrimMonad (PrimState))
import Data.Vector.Mutable qualified as MVector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Data.Word (Word64)
import GHC.Exts qualified as GHC
import Plutarch.Backend.AST
  ( AST
      ( ASTApply,
        ASTCase,
        ASTCompose,
        ASTConstr,
        ASTDelay,
        ASTFix,
        ASTForce,
        ASTLam,
        ASTLeaf
      ),
    Hash,
  )
import Plutarch.Backend.AST qualified as AST
import Plutarch.Backend.UPLC (UPLCTerm)
import Plutarch.Helpers.Pretty (customSet, prettyValueOf, (<:=>))
import PlutusCore (Some (Some), ValueOf (ValueOf))
import PlutusCore qualified as PLC
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    align,
    braces,
    brackets,
    group,
    hardline,
    hsep,
    indent,
    list,
    punctuate,
    vcat,
    viaShow,
    (<+>),
  )

-- | A hash identifying a bound variable argument, together with its
-- multiplicity (how many times it occurs in the body where it is bound).
--
-- @since 2.0.0
data BoundVar = BoundVar Hash Multiplicity
  deriving stock
    ( -- | @since 2.0.0
      Eq,
      -- | @since 2.0.0
      Show
    )

-- | @since 2.0.0
instance Pretty BoundVar where
  pretty (BoundVar h m) = pretty h <> ":" <> pretty m

-- | A positive-only number indicating the number of times a bound variable is
-- used in the body of whatever binds it.
--
-- @since 2.0.0
newtype Multiplicity = Multiplicity Word
  deriving stock
    ( -- | @since 2.0.0
      Show
    )
  deriving
    ( -- | @since 2.0.0
      Eq,
      -- | @since 2.0.0
      Pretty
    )
    via Word
  deriving
    ( -- | @since 2.0.0
      Semigroup
    )
    via (Sum Word)

-- | The multiplicity of one.
--
-- @since 2.0.0
pattern MultOne :: Multiplicity
pattern MultOne = Multiplicity 1

-- | A leaf bind in the ANF (that is, one that cannot have dependencies).
--
-- @since 2.0.0
data Leaf (ann :: Type)
  = LConstant ann (Some (ValueOf PLC.DefaultUni))
  | LBuiltin ann PLC.DefaultFun
  | LCompiled ann UPLCTerm
  | LError ann
  deriving stock
    ( -- | @since 2.0.0
      Functor,
      -- | @since 2.0.0
      Show,
      -- | @since 2.0.0
      Eq
    )

-- | @since 2.0.0
instance Pretty (Leaf ann) where
  pretty = \case
    LConstant _ (Some (ValueOf uni x)) -> prettyValueOf uni x
    LBuiltin _ fun -> viaShow fun
    LCompiled _ uplc -> "COMPILED" <+> align (braces (align . group $ pretty uplc))
    LError _ -> "ERROR"

-- | As ANF \'inlines\' variables, subcomputations are either variables
-- (identified by their hashes) or other binds (identified by their
-- identifiers).
--
-- @since 2.0.0
data Ref
  = AVar Hash
  | AnId Id
  deriving stock
    ( -- | @since 2.0.0
      Show,
      -- | @since 2.0.0
      Eq
    )

-- | @since 2.0.0
instance Pretty Ref where
  pretty = \case
    AVar h -> pretty h
    AnId i -> pretty i

-- | An identifier for an ANF bind.
--
-- @since 2.0.0
newtype Id = Id Int
  deriving
    ( -- | @since 2.0.0
      Eq,
      -- | @since 2.0.0
      Ord
    )
    via Int
  deriving stock
    ( -- | @since 2.0.0
      Show
    )

-- | @since 2.0.0
instance Pretty Id where
  pretty (Id i) = "#" <> viaShow i

-- | An ANF bind. Instead of a recursive ANF-like structure, this uses 'Id' to
-- determine subcomputations.
--
-- @since 2.0.0
data ANFBind (ann :: Type)
  = ANFLeaf (Leaf ann)
  | ANFForce ann Ref
  | ANFDelay ann Ref
  | ANFLam ann (NonEmptyVector (Maybe BoundVar)) Ref
  | ANFFix ann BoundVar Ref
  | ANFApply ann Ref (NonEmptyVector Ref)
  | ANFConstr ann Word64 (Vector Ref)
  | ANFCase ann Ref (NonEmptyVector Ref)
  | ANFCompose ann (NonEmptyVector Ref)
  deriving stock
    ( -- | @since 2.0.0
      Show,
      -- | @since 2.0.0
      Functor,
      -- | @since 2.0.0
      Eq
    )

-- | @since 2.0.0
instance Pretty (ANFBind ann) where
  pretty = \case
    ANFLeaf l -> pretty l
    ANFForce _ ref -> "Force" <+> pretty ref
    ANFDelay _ ref -> "Delay" <+> pretty ref
    ANFLam _ args body -> "\\" <> mkArgs args <+> "->" <+> pretty body
    ANFFix _ mult body -> "Fix" <> brackets (pretty mult) <+> pretty body
    ANFApply _ fnRef args -> "Apply" <+> pretty fnRef <+> list (pretty <$> NEVector.toList args)
    ANFConstr _ cix args -> "Constr" <+> viaShow cix <+> list (pretty <$> Vector.toList args)
    ANFCase _ scrut handlers -> "Case" <+> pretty scrut <+> list (pretty <$> NEVector.toList handlers)
    ANFCompose _ args -> hsep . punctuate " <<<" . fmap pretty . NEVector.toList $ args
    where
      mkArgs :: forall (ann' :: Type). NEVector.NonEmptyVector (Maybe BoundVar) -> Doc ann'
      mkArgs (NEVector.toList -> xs) =
        hsep
          . fmap (\case Nothing -> "_"; Just m -> pretty m)
          $ xs

-- | @since 2.0.0
getANFBindAnn :: forall (ann :: Type). ANFBind ann -> ann
getANFBindAnn = \case
  ANFLeaf ell -> case ell of
    LConstant x _ -> x
    LBuiltin x _ -> x
    LCompiled x _ -> x
    LError x -> x
  ANFForce x _ -> x
  ANFDelay x _ -> x
  ANFLam x _ _ -> x
  ANFFix x _ _ -> x
  ANFApply x _ _ -> x
  ANFConstr x _ _ -> x
  ANFCase x _ _ -> x
  ANFCompose x _ -> x

-- | A combination of a (nonempty) vector of binds, together with a unique
-- mapping between identifiers and hashes of unique subcomputations.
--
-- @since 2.0.0
data ANF (ann :: Type) = ANF (Bimap Id Hash) (NonEmptyVector (ANFBind ann))

-- | @since 2.0.0
instance Pretty (ANF ()) where
  pretty anf =
    "ANF Hashes:"
      <> hardline
      <> indent 1 (align $ prettyANFHashes anf)
      <> hardline
      <> hardline
      <> "ANF Binds:"
      <> hardline
      <> indent 1 (align $ prettyANFBinds anf)
      <> hardline

-- | @since 2.0.0
instance {-# OVERLAPS #-} (Pretty ann) => Pretty (ANF ann) where
  pretty anf =
    "ANF Hashes:"
      <> hardline
      <> indent 1 (align $ prettyANFHashes anf)
      <> hardline
      <> hardline
      <> "ANF Binds:"
      <> hardline
      <> indent 1 (align $ prettyANFBinds anf)
      <> hardline
      <> hardline
      <> "ANF Annotations:"
      <> hardline
      <> indent 1 (align $ prettyANFAnnotations anf)
      <> hardline

-- | Given an 'AST' annotated with hashes for unique (up to alpha-equivalence)
-- subcomputations, construct an ANF. The 'Hash' annotations are used to
-- identify alpha-equivalent subcomputations, so that they correspond to a
-- single unique bind.
--
-- @since 2.0.0
fromHashedAST :: AST Hash -> ANF ()
fromHashedAST ast = case runState (go ast) (Bimap.empty, IntMap.empty) of
  -- Note (Koz, 05/06/2026): Due to how we construct the IntMap here, there is
  -- no chance of 'missing' or non-contiguous keys. Thus, the `lookup` cannot
  -- fail.
  (_, (bm, im)) -> ANF bm . NEVector.generate1 (IntMap.size im) $ \i -> fromJust . IntMap.lookup i $ im
  where
    -- We maintain the partially-constructed bind list as an `IntMap` mostly for
    -- efficiency, as incrementally expanding a `Vector` is quite tedious and
    -- inefficient.
    go :: AST Hash -> State (Bimap Id Hash, IntMap (ANFBind ())) Ref
    go = \case
      ASTLeaf ell -> doLeaf ell
      ASTForce h body -> withLookup h $ do
        bodyRef <- go body
        newBind h (ANFForce () bodyRef)
      ASTDelay h body -> withLookup h $ do
        bodyRef <- go body
        newBind h (ANFDelay () bodyRef)
      ASTLam h bvs body -> withLookup h $ do
        bodyRef <- go body
        boundVars <- traverse (traverse (bvWithMultiplicity bodyRef)) bvs
        newBind h (ANFLam () boundVars bodyRef)
      ASTFix h bv body -> withLookup h $ do
        bodyRef <- go body
        boundVar <- bvWithMultiplicity bodyRef bv
        newBind h (ANFFix () boundVar bodyRef)
      ASTApply h f xs -> withLookup h $ do
        fRef <- go f
        xsRefs <- traverse go xs
        newBind h (ANFApply () fRef xsRefs)
      ASTConstr h tag fields -> withLookup h $ do
        fieldsRefs <- traverse go fields
        newBind h (ANFConstr () tag fieldsRefs)
      ASTCase h scrut handlers -> withLookup h $ do
        scrutRef <- go scrut
        handlersRefs <- traverse go handlers
        newBind h (ANFCase () scrutRef handlersRefs)
      ASTCompose h components -> withLookup h $ do
        componentsRefs <- traverse go components
        newBind h (ANFCompose () componentsRefs)
    doLeaf :: AST.Leaf Hash -> State (Bimap Id Hash, IntMap (ANFBind ())) Ref
    doLeaf = \case
      AST.LVar _ h -> pure . AVar $ h
      AST.LConstant h c -> withLookup h $ newBind h (ANFLeaf (LConstant () c))
      AST.LBuiltin h f -> withLookup h $ newBind h (ANFLeaf (LBuiltin () f))
      AST.LCompiled h code -> withLookup h $ newBind h (ANFLeaf (LCompiled () code))
      AST.LError h -> withLookup h $ newBind h (ANFLeaf (LError ()))
    -- Checks if we've already seen an alpha-equivalent bind. If so, yield a
    -- `Ref` to it, otherwise take the supplied action to build it, and produce
    -- the `Ref` from that.
    withLookup ::
      Hash ->
      State (Bimap Id Hash, IntMap (ANFBind ())) Ref ->
      State (Bimap Id Hash, IntMap (ANFBind ())) Ref
    withLookup h act = do
      mId <- gets (Bimap.lookupR h . fst)
      maybe act (pure . AnId) mId
    newBind :: Hash -> ANFBind () -> State (Bimap Id Hash, IntMap (ANFBind ())) Ref
    newBind h bind = do
      -- Ensures we have contiguous keys starting from 0.
      firstAvailable <- gets (maybe 0 ((+ 1) . fst) . IntMap.lookupMax . snd)
      let asId = Id firstAvailable
      modify (bimap (Bimap.insert asId h) (IntMap.insert firstAvailable bind))
      pure . AnId $ asId

-- | Indicates dependency chains. More precisely, ANF binds with a higher rank
-- have dependencies whose rank is lower.
--
-- @since 2.0.0
newtype Rank = Rank Word
  deriving
    ( -- | @since 2.0.0
      Eq,
      -- | @since 2.0.0
      Ord
    )
    via Word
  deriving stock
    ( -- | @since 2.0.0
      Show
    )

-- | @since 2.0.0
instance Pretty Rank where
  {-# INLINEABLE pretty #-}
  pretty = viaShow

-- | Describes whether a given ANF bind will be `let`-bound later or not.
--
-- @since 2.0.0
data BindingStatus = ShouldBind | Shouldn'tBind
  deriving stock
    ( -- | @since 2.0.0
      Show,
      -- | @since 2.0.0
      Eq
    )

-- | @since 2.0.0
instance Pretty BindingStatus where
  pretty = \case
    ShouldBind -> "Let-bound"
    Shouldn'tBind -> "Inlined"

-- | Annotation for ANF binds with two pieces of information:
--
-- * Whether this bind will be `let`-bound later
-- * What, if anything, should be `let`-bound here, and what dependency
--   structure these `let`-binds have to have
--
-- @since 2.0.0
data Analysis = Analysis BindingStatus (Map Rank (NESet Id))
  deriving stock
    ( -- | @since 2.0.0
      Show,
      -- | @since 2.0.0
      Eq
    )

-- | @since 2.0.0
instance Pretty Analysis where
  pretty (Analysis status ranking) =
    pretty status
      <> ", required let-binds here: "
      <> customSet (fmap prettyKV . Map.toList $ ranking)
    where
      prettyKV :: forall (ann :: Type). (Rank, NESet Id) -> Doc ann
      prettyKV (r, bindings) = pretty r <> " -> " <> customSet (fmap pretty . GHC.toList $ bindings)

-- | @since 2.0.0
newtype Dependencies = Dependencies (Maybe (NESet Id, Set Id))
  deriving
    ( -- | @since 2.0.0
      Eq
    )
    via (Maybe (NESet Id, Set Id))

-- | @since 2.0.0
instance Pretty Dependencies where
  pretty (Dependencies deps) = case deps of
    Nothing -> "No dependencies"
    Just (dir, indir) ->
      "Direct: "
        <> customSet (NonEmpty.toList . fmap pretty . NESet.toList $ dir)
        <> ", indirect: "
        <> customSet (fmap pretty . Set.toAscList $ indir)

-- | @since 2.0.0
pattern NoDeps :: Dependencies
pattern NoDeps = Dependencies Nothing

-- | @since 2.0.0
pattern Deps :: NESet Id -> Set Id -> Dependencies
pattern Deps dir indir <- Dependencies (Just (dir, indir))

{-# COMPLETE NoDeps, Deps #-}

-- | Performs dependency analysis.
--
-- @since 2.0.0
analyzeDeps :: forall (ann :: Type). ANF ann -> ANF Dependencies
analyzeDeps (ANF hashes binds) = ANF hashes . getAllDeps $ binds

-- | Performs all the analysis needed for code generation to consume the ANF.
--
-- @since 2.0.0
fullPipeline :: ANF Dependencies -> ANF Analysis
fullPipeline (ANF hashes binds) =
  let onlyDeps = NEVector.map getANFBindAnn binds
      -- Check which other binds want every given bind as a direct dependency
      whereDemanded = determineDemandSites onlyDeps
      -- Since we only consider `let`-binding something that needs to be used more
      -- than once, throw out any singleton-valued entries
      candidates = Map.filterWithKey isCandidate whereDemanded
      -- Candidates may need variables in scope to be meaningful, so identify
      -- them.
      candidateNeededVars = Map.mapWithKey (\i _ -> collectVarDeps binds i) candidates
      -- As an ANF is a closed expression, every variable gets brought into
      -- scope somewhere. Additionally, all variables have unique names, which
      -- means every variable we use anywhere is brought into scope in _exactly_
      -- one place.
      varIntroLocations = findVarIntroLocations binds
      -- Using a combination of what vars are needed where, and what vars get
      -- bound where, we can determine a 'bind site of last resort' for every
      -- candidate. If a candidate doesn't need any variables bound, its 'bind
      -- site of last resort' is the topmost computation.
      bindSites = findBindSites varIntroLocations (Id $ NEVector.length binds - 1) candidateNeededVars
      -- Bind sites of last resort are rarely optimal, due to the strictness of
      -- UPLC evaluation. Consider the following example:
      --
      -- \a -> if a then doB else doC
      --
      -- Suppose that `doB` (and only `doB`) depends on a closed subcomputation
      -- `c1` multiple times. Likewise, suppose that `doC` (and only `doC`)
      -- depends on a closed subcomputation `c2` multiple times. If we use the
      -- site of last resort as the `let`-binding location, we end up compiling
      -- something like
      --
      -- let c1 == ...
      --     c2 == ...
      --   in \a -> if a then doB' else doC'
      --
      -- where `doB'` and `doC'` are `doB` and `doC` respectively, with the
      -- relevant subcomputations replaced by `let`-bound variables. However,
      -- this would force both `c1` and `c2` to be evaluated, even though we
      -- only ever need one of them!
      --
      -- A more sensible strategy would be to compile something like this
      -- instead:
      --
      -- \a -> if a then (let c1 = ... in doB') else (let c2 = ... in doC')
      --
      -- In this setting, we only evaluate whichever of `c1` and `c2` ends up
      -- being required.
      --
      -- To enable this, we need a 'digging' analysis to try and 'push'
      -- `let`-bindings as close to their use sites as possible. For this, we
      -- need to know all dependencies, not just direct ones, for every bind.
      idealBindSites = Map.mapWithKey (optimizeBindSite onlyDeps) bindSites
      -- Reorganize our information to be keyed by _site_, rather than what is
      -- being bound. One site may be the place for multiple bindings.
      bindingsBySite = toBindingsBySite idealBindSites
      -- If we have multiple bindings at the same site, these may in turn depend
      -- on each other. To ensure that this doesn't break at codegen time, we
      -- need to rank such bindings, so that we can ensure any bindings that
      -- depend on future bindings are bound first
      rankedBindings = Map.map (rankBindings onlyDeps) bindingsBySite
   in ANF hashes . NEVector.imap (analyze (Map.keysSet candidates) rankedBindings) $ binds
  where
    isCandidate :: Id -> NESet Id -> Bool
    isCandidate (Id bindIx) whereNeeded =
      let neededMoreThanOnce = NESet.size whereNeeded > 1
       in case binds NEVector.! bindIx of
            ANFLeaf ell -> case ell of
              -- Small constants should never be considered candidates
              LConstant _ c -> case c of
                Some (ValueOf PLC.DefaultUniBool _) -> False
                Some (ValueOf PLC.DefaultUniUnit _) -> False
                Some (ValueOf PLC.DefaultUniInteger n) -> abs n >= 256 && neededMoreThanOnce
                _ -> neededMoreThanOnce
              -- Builtins should never be considered candidates
              LBuiltin _ _ -> False
              -- The error node should never be a candidate
              LError _ -> False
              _ -> neededMoreThanOnce
            _ -> neededMoreThanOnce

-- Helpers

rankBindings ::
  NonEmptyVector Dependencies ->
  NESet Id ->
  NEMap Rank (NESet Id)
rankBindings onlyDeps bindings = case NESet.deleteFindMin bindings of
  (lowestId, rest) ->
    let start = NEMap.singleton (Rank 0) . NESet.singleton $ lowestId
     in Set.foldl' go start rest
  where
    go :: NEMap Rank (NESet Id) -> Id -> NEMap Rank (NESet Id)
    go acc letBinding@(Id letIx) =
      let toInsert = NESet.singleton letBinding
       in case onlyDeps NEVector.! letIx of
            NoDeps -> NEMap.insertWith NESet.union (Rank 0) toInsert acc
            Deps dir indir ->
              let combinedDeps = GHC.toList . Set.union (NESet.toSet dir) $ indir
               in case mapMaybe (rankOf acc) . GHC.toList $ combinedDeps of
                    [] -> NEMap.insertWith NESet.union (Rank 0) toInsert acc
                    ranks ->
                      let Rank maxRank = maximum ranks
                       in NEMap.insertWith NESet.union (Rank $ maxRank + 1) toInsert acc
    -- Inefficient 'reverse lookup'
    rankOf :: NEMap Rank (NESet Id) -> Id -> Maybe Rank
    rankOf alreadyRanked i =
      NEMap.foldrWithKey
        ( \r ranked acc ->
            if NESet.member i ranked
              then case acc of
                Nothing -> Just r
                Just r' -> Just (max r r')
              else acc
        )
        Nothing
        alreadyRanked

toBindingsBySite :: Map Id Id -> Map Id (NESet Id)
toBindingsBySite = Map.foldrWithKey go Map.empty
  where
    go :: Id -> Id -> Map Id (NESet Id) -> Map Id (NESet Id)
    go bind site = Map.insertWith NESet.union site . NESet.singleton $ bind

-- Construct the final analysis.
analyze ::
  Set Id ->
  Map Id (NEMap Rank (NESet Id)) ->
  Int ->
  ANFBind Dependencies ->
  ANFBind Analysis
analyze candidates rankedBindings bindIx bind = case Map.lookup (Id bindIx) rankedBindings of
  Nothing -> Analysis status Map.empty <$ bind
  Just toLetBind -> Analysis status (NEMap.toMap toLetBind) <$ bind
  where
    status :: BindingStatus
    status =
      if Set.member (Id bindIx) candidates
        then ShouldBind
        else Shouldn'tBind

optimizeBindSite :: NonEmptyVector Dependencies -> Id -> Id -> Id
optimizeBindSite allDeps target currSite@(Id siteIx) = case allDeps NEVector.! siteIx of
  -- Should be impossible - such a bind is either a leaf or 'full of variables',
  -- both of which are ineligible for any `let`-binding.
  NoDeps ->
    error $
      "Candidate site for a let-binding has no dependencies.\n"
        <> "If you see this, please report it as a bug.\n"
        <> "Target: "
        <> show target
        <> "\nCurrent site: "
        <> show currSite
  Deps dir _ ->
    if NESet.member target dir
      -- If the bind is our direct dependency, we _must_ `let`-bind here.
      then currSite
      else case NESet.size dir of
        -- If we have a single direct dependency only, we are never an optimal
        -- bind site, as we cound bind at that dependency instead.
        1 -> optimizeBindSite allDeps target . NESet.findMin $ dir
        -- If we have multiple direct dependencies, by our earlier analysis, we
        -- know that at least one of these dependencies itself depends on
        -- `target`. There are two caes:
        --
        -- 1. There is exactly one such dependency; or
        -- 2. There is more than one such dependency.
        --
        -- The first time we encounter Case 2 is the optimal bind site. If we
        -- see Case 1, then that dependency (or one of its dependencies) is
        -- instead a better choice
        _ ->
          let suitableDeps = NESet.filter isSuitable dir
           in case Set.size suitableDeps of
                1 -> optimizeBindSite allDeps target . Set.findMin $ suitableDeps
                _ -> currSite
  where
    isSuitable :: Id -> Bool
    isSuitable (Id candidateIx) = case allDeps NEVector.! candidateIx of
      NoDeps -> False
      Deps dir indir -> NESet.member target dir || Set.member target indir

getAllDeps ::
  forall (ann :: Type).
  NonEmptyVector (ANFBind ann) ->
  NonEmptyVector (ANFBind Dependencies)
getAllDeps binds = NEVector.unsafeCreate $ do
  let len = NEVector.length binds
  mv <- MVector.new len
  for_ [0, 1 .. len - 1] $ \i -> do
    let bind = binds NEVector.! i
    case bind of
      -- Leaves never have dependencies, transitive or otherwise.
      ANFLeaf ell -> MVector.write mv i $ ANFLeaf (NoDeps <$ ell)
      -- Non-leaves may have dependencies.
      ANFForce _ r -> do
        mDeps <- refToDeps mv r
        MVector.write mv i (ANFForce mDeps r)
      ANFDelay _ r -> do
        mDeps <- refToDeps mv r
        MVector.write mv i (ANFDelay mDeps r)
      ANFLam _ bvs body -> do
        mDeps <- refToDeps mv body
        MVector.write mv i (ANFLam mDeps bvs body)
      ANFFix _ bv body -> do
        mDeps <- refToDeps mv body
        MVector.write mv i (ANFFix mDeps bv body)
      -- To ensure that later stages don't break, the sets of direct and
      -- indirect dependencies must be kept disjoint for every bind. For
      -- single-descendant binds this is not an issue, but for join points, we
      -- have to be careful.
      ANFApply _ f xs -> do
        mFDeps <- refToDeps mv f
        allDeps <- NEVector.foldM (combineDeps mv) mFDeps xs
        MVector.write mv i (ANFApply allDeps f xs)
      ANFConstr _ tag fields -> do
        allDeps <- Vector.foldM (combineDeps mv) NoDeps fields
        MVector.write mv i (ANFConstr allDeps tag fields)
      ANFCase _ scrut handlers -> do
        mScrutDeps <- refToDeps mv scrut
        allDeps <- NEVector.foldM (combineDeps mv) mScrutDeps handlers
        MVector.write mv i (ANFCase allDeps scrut handlers)
      ANFCompose _ components -> do
        allDeps <- NEVector.foldM (combineDeps mv) NoDeps components
        MVector.write mv i (ANFCompose allDeps components)
  pure mv
  where
    combineDeps ::
      forall (m :: Type -> Type).
      (PrimMonad m) =>
      MVector (PrimState m) (ANFBind Dependencies) ->
      Dependencies ->
      Ref ->
      m Dependencies
    combineDeps mv acc@(Dependencies deps) r = do
      Dependencies mDeps <- refToDeps mv r
      case deps of
        Nothing -> pure . Dependencies $ mDeps
        Just (dir, indir) -> case mDeps of
          Nothing -> pure acc
          -- We have to ensure that the sets of direct and indirect dependencies
          -- remain disjoint.
          Just (dir', indir') -> do
            let newDir = NESet.union dir dir'
            let newIndir = Set.difference (Set.union indir indir') (NESet.toSet newDir)
            pure . Dependencies . Just $ (newDir, newIndir)
    refToDeps ::
      forall (m :: Type -> Type).
      (PrimMonad m) =>
      MVector (PrimState m) (ANFBind Dependencies) ->
      Ref ->
      m Dependencies
    refToDeps mv = \case
      -- Vars never contribute dependencies
      AVar _ -> pure NoDeps
      -- This read is always safe, as we traverse indexes from low to high.
      -- As lower-indexed `Id`s are dependencies of higher-indexed `Id`s, any
      -- bind we might need to ask about has already been processed.
      --
      -- The given `Id` becomes a direct dep, and any of _its_ deps become
      -- transitive. Since binds cannot self-refer, we can do this directly.
      AnId i@(Id ix) -> do
        Dependencies transitive <- getANFBindAnn <$> MVector.read mv ix
        let newTransitive = maybe Set.empty transify transitive
        pure . Dependencies . Just $ (NESet.singleton i, newTransitive)
    -- We maintain the invariant that direct and indirect dependencies of any
    -- bind are disjoint. Thus, we can just chuck everything together and not
    -- worry about it.
    transify :: (NESet Id, Set Id) -> Set Id
    transify (dir, indir) = Set.union (NESet.toSet dir) indir

determineDemandSites ::
  NonEmptyVector Dependencies ->
  Map Id (NESet Id)
determineDemandSites = NEVector.foldl1' (Map.unionWith NESet.union) . NEVector.imap toDepMap
  where
    toDepMap :: Int -> Dependencies -> Map Id (NESet Id)
    toDepMap currIdIx = \case
      NoDeps -> Map.empty
      Deps dirDeps _ -> Map.fromSet (const (NESet.singleton . Id $ currIdIx)) . NESet.toSet $ dirDeps

findBindSites ::
  Map Hash Id ->
  Id ->
  Map Id (Set Hash) ->
  Map Id Id
findBindSites varIntroLocations topComp = Map.map (Set.foldr combine topComp)
  where
    -- We are trying to find the `Id` corresponding to the 'innermost' lambda
    -- that binds variables that we need. This means we want the _lowest_
    -- possible `Id`, as lower `Id`s are dependencies of higher `Id`s.
    combine :: Hash -> Id -> Id
    combine h (Id acc) = case Map.lookup h varIntroLocations of
      -- This should be impossible.
      Nothing ->
        error $
          "A variable is introduced but no introduction site was found for it.\n"
            <> "If you see this, please report it as an error!\n"
            <> "Var: "
            <> show h
      Just (Id loc) -> Id (min loc acc)

findVarIntroLocations ::
  forall (ann :: Type).
  NonEmptyVector (ANFBind ann) ->
  Map Hash Id
findVarIntroLocations = NEVector.ifoldl' go Map.empty
  where
    go :: Map Hash Id -> Int -> ANFBind ann -> Map Hash Id
    go acc i = \case
      -- Lambdas and fixes are the only binders, so collect all their variables
      -- and note they're bound here.
      ANFLam _ bvs _ -> NEVector.foldl' (\acc' -> maybe acc' (insertBV acc' (Id i))) acc bvs
      ANFFix _ bv _ -> insertBV acc (Id i) bv
      -- Nothing else binds, so skip
      _ -> acc
    insertBV :: Map Hash Id -> Id -> BoundVar -> Map Hash Id
    insertBV acc i (BoundVar h _) = Map.insert h i acc

collectVarDeps ::
  forall (ann :: Type).
  NonEmptyVector (ANFBind ann) ->
  Id ->
  Set Hash
collectVarDeps binds (Id i) = case binds NEVector.! i of
  -- Leaves never contain variables.
  ANFLeaf _ -> Set.empty
  ANFForce _ r -> refToVars r
  ANFDelay _ r -> refToVars r
  -- Exclude any variables bound by lambdas and fixpoints, as we're interested
  -- in free variables relative the given expression only.
  ANFLam _ bvs body ->
    let bodyVars = refToVars body
     in Set.difference bodyVars . bvsToHashes $ bvs
  ANFFix _ bv body -> Set.delete (bvToHash bv) . refToVars $ body
  ANFApply _ f xs -> NEVector.foldl' combine (refToVars f) xs
  ANFConstr _ _ fields -> Vector.foldl' combine Set.empty fields
  ANFCase _ scrut handlers -> NEVector.foldl' combine (refToVars scrut) handlers
  ANFCompose _ components -> NEVector.foldl' combine Set.empty components
  where
    refToVars :: Ref -> Set Hash
    refToVars = \case
      AVar h -> Set.singleton h
      AnId j -> collectVarDeps binds j
    combine :: Set Hash -> Ref -> Set Hash
    combine acc = Set.union acc . refToVars
    bvToHash :: BoundVar -> Hash
    bvToHash (BoundVar h _) = h
    bvsToHashes :: NonEmptyVector (Maybe BoundVar) -> Set Hash
    bvsToHashes = NEVector.foldl' (\acc -> maybe acc (\bv -> Set.insert (bvToHash bv) acc)) Set.empty

bvWithMultiplicity ::
  forall (a :: Type) (m :: Type -> Type).
  (MonadState (a, IntMap (ANFBind ())) m) =>
  Ref -> Hash -> m BoundVar
bvWithMultiplicity r h = case r of
  AVar h' ->
    if h' == h
      then pure . BoundVar h . Multiplicity $ 1
      else error "Argument claimed as used, but body shows it as unused. If you see this, report a bug."
  AnId _ -> do
    existingBinds <- gets snd
    let (Sum mult) = foldMap countOccurrence existingBinds
    pure . BoundVar h . Multiplicity $ mult
    where
      countOccurrence :: ANFBind () -> Sum Word
      countOccurrence = \case
        ANFLeaf _ -> mempty
        ANFForce _ body -> hashToCount h body
        ANFDelay _ body -> hashToCount h body
        ANFLam _ _ body -> hashToCount h body
        ANFFix _ _ body -> hashToCount h body
        ANFApply _ f xs -> hashToCount h f <> foldMap (hashToCount h) xs
        ANFConstr _ _ fields -> foldMap (hashToCount h) fields
        ANFCase _ scrut handlers -> hashToCount h scrut <> foldMap (hashToCount h) handlers
        ANFCompose _ components -> foldMap (hashToCount h) components

hashToCount :: Hash -> Ref -> Sum Word
hashToCount h = \case
  AVar h' -> if h == h' then Sum 1 else mempty
  AnId _ -> mempty

prettyANFBinds :: forall ann1 ann2. ANF ann1 -> Doc ann2
prettyANFBinds (ANF _ binds) = vcat . NEVector.toList $ NEVector.imap (\(Id -> i) b -> mkBind i b) binds
  where
    mkBind :: Id -> ANFBind ann1 -> Doc ann2
    mkBind i b = align . group $ pretty i <:=> align (group $ pretty b)

prettyANFAnnotations :: forall ann1 ann2. (Pretty ann1) => ANF ann1 -> Doc ann2
prettyANFAnnotations (ANF _ binds) = vcat . NEVector.toList $ NEVector.imap (\(Id -> i) b -> mkAnn i b) binds
  where
    mkAnn :: Id -> ANFBind ann1 -> Doc ann2
    mkAnn i b = align . group $ pretty i <:=> align (group . pretty $ getANFBindAnn b)

prettyANFHashes :: forall ann1 ann2. ANF ann1 -> Doc ann2
prettyANFHashes (ANF hashes _) = vcat . map (\(i, h) -> pretty i <:=> pretty h) . Bimap.toAscList $ hashes
