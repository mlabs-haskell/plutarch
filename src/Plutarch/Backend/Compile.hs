{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Generates 'UPLCTerm' from an 'ANF'.
--
-- @since 2.0.0
module Plutarch.Backend.Compile
  ( toUPLCTerm,
  )
where

import Control.Monad (foldM)
import Control.Monad.RWS.CPS
  ( MonadState (get),
    RWS,
    asks,
    evalRWS,
    modify,
    runRWS,
  )
import Control.Monad.Reader (MonadReader)
import Control.Monad.State.Strict
  ( State,
    gets,
    put,
    runState,
  )
import Data.Foldable (foldl', foldrM)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Data.Text (Text)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Backend.ANF
  ( ANF (ANF),
    ANFBind
      ( ANFApply,
        ANFCase,
        ANFCompose,
        ANFConstr,
        ANFDelay,
        ANFFix,
        ANFForce,
        ANFLam,
        ANFLeaf
      ),
    Analysis (Analysis),
    BindingStatus (ShouldBind, Shouldn'tBind),
    BoundVar (BoundVar),
    Id (Id),
    Leaf
      ( LBuiltin,
        LCompiled,
        LConstant,
        LError
      ),
    Rank,
    Ref (AVar, AnId),
    getANFBindAnn,
  )
import Plutarch.Backend.AST
  ( Hash (Hash),
  )
import Plutarch.Backend.UPLC
  ( UPLCTerm,
    optimizeUplc,
    rewriteUniques,
    substituteVarFor,
    uplcApply,
    uplcApply1,
    uplcBuiltin,
    uplcCase,
    uplcConstant,
    uplcConstr,
    uplcDelay,
    uplcError,
    uplcForce,
    uplcLam,
    uplcLam1,
    uplcLet,
    uplcLetP,
    uplcMCombinator,
    uplcVar,
  )
import Plutarch.Helpers.Backend (getFresh)
import Plutarch.Helpers.Evaluate (evalUPLC, maxBudget)
import PlutusCore qualified as PLC
import UntypedPlutusCore qualified as UPLC

-- | Given an ANF, compile it into UPLC. This compilation also applies automatic
-- @let@-bindings of any unique (up to alpha-equivalence) computation that is
-- used more than once.
--
-- @since 2.0.0
toUPLCTerm :: Bool -> ANF Analysis -> UPLCTerm
toUPLCTerm runExternalOpts (ANF _ binds) =
  let -- We use the hashes of any variable as its `Unique`. To ensure we don't
      -- accidentally ever alias them, we collect all the ones we use. As we
      -- know they can't collide, we don't perform any rehashing.
      allVarUniques = NEVector.foldl' collectVarName Set.empty binds
      -- Check if any precompiled code sections have name clashes with our
      -- variables, and if so, rehash to fix them.
      (rewrittenBinds, usedNames) = fixPrecompiled binds allVarUniques
      -- See if we have the identity function somewhere. If we have it, it's
      -- unique due to hash consing.
      mIdentity = findIdentity rewrittenBinds
      -- Find all fixpoints, record their locations.
      fixpoints = doFixpointAnalysis rewrittenBinds
      -- To compile a fixpoint, we take its functional (of the form `F = \self
      -- -> body`) and transform it into `M (\r -> F (r r))`. As `M` is small,
      -- it's cheaper to inline it than make a dedicated `let`-binding.
      --
      -- Since every fixpoint bind already contains a (unique) name for its
      -- `self` argument, we can re-use it for `r` in the above construction.
      -- Since we are inlining `M`, and fixpoints may contain more nested
      -- fixpoints, we need a unique name for each fixpoint we will be
      -- compiling.
      (fixpointNameMap, lastFresh, _) = runRWS (foldM mkFixpointName Map.empty fixpoints) usedNames 0
      -- Find all function compositions, record their locations
      compositions = doCompositionAnalysis rewrittenBinds
      -- To compile a composition of the form `[f_1, f_2, ... , f_k]`, we want
      -- to produce `\z -> f_1 (f_2 (... (f_k z) ...)`. For this, we need a
      -- unique name for `z` for each composition.
      (compositionNameMap, lastFresh', _) = runRWS (foldM mkCompositionName Map.empty compositions) usedNames lastFresh
      -- Make a unique name for any unused arguments. As lambdas in UPLC are all
      -- arity 1, and we will never use an unused argument, we can generate one
      -- name for all of them. It's cheaper to do this speculatively than scan
      -- for unused function parameters.
      (unusedParamName, lastFresh'', _) = runRWS mkUnusedName usedNames lastFresh'
      -- Name every bind, avoiding any names of existing variables.
      namedBinds = fst . evalRWS (NEVector.mapM nameBind rewrittenBinds) usedNames $ lastFresh''
      -- Set up our compilation environment with everything we just put
      -- together.
      compileEnv = CompileEnv namedBinds fixpointNameMap compositionNameMap unusedParamName mIdentity
      compileState = CompileState Map.empty Set.empty
      -- Compile all binds
      compiled = runCompileM compile compileEnv compileState
   in if
        | not runExternalOpts -> compiled
        -- If there are no `let`-binds, we can pre-evaluate safely
        | isAffine -> case optimizeUplc . preEval $ compiled of
            Left err -> freeVarErr err
            Right res -> res
        -- Only run UPLC optimizations
        | otherwise -> case optimizeUplc compiled of
            Left err -> freeVarErr err
            Right res -> res
  where
    collectVarName :: Set Int -> ANFBind Analysis -> Set Int
    collectVarName acc = \case
      ANFLeaf _ -> acc
      ANFForce _ r -> addVar acc r
      ANFDelay _ r -> addVar acc r
      ANFLam _ _ body -> addVar acc body
      ANFFix _ _ body -> addVar acc body
      ANFApply _ f xs -> addVar (NEVector.foldl' addVar acc xs) f
      ANFConstr _ _ fields -> Vector.foldl' addVar acc fields
      ANFCase _ scrut handlers -> addVar (NEVector.foldl' addVar acc handlers) scrut
      ANFCompose _ components -> NEVector.foldl' addVar acc components
    addVar :: Set Int -> Ref -> Set Int
    addVar ess = \case
      AVar (Hash h) -> Set.insert h ess
      _ -> ess
    mkUnusedName :: RWS (Set Int) () Int PLC.Name
    mkUnusedName = do
      fresh <- untilM getFresh (asks . Set.notMember)
      pure . mkName "unused" $ fresh
    nameBind :: ANFBind Analysis -> RWS (Set Int) () Int (PLC.Name, ANFBind Analysis)
    nameBind bind = do
      fresh <- untilM getFresh (asks . Set.notMember)
      pure (mkName "bind" fresh, bind)
    isAffine :: Bool
    isAffine = NEVector.all ((\(Analysis shouldBind _) -> shouldBind == Shouldn'tBind) . getANFBindAnn) binds
    preEval :: UPLCTerm -> UPLCTerm
    preEval t = case evalUPLC maxBudget t of
      (res, _, _) -> case res of
        Left err -> error $ "Pre-evaluation errored. If you see this, report a bug: " <> show err
        Right (Left c) -> uplcConstant c
        Right (Right t') -> t'
    freeVarErr :: forall (a :: Type). UPLC.FreeVariableError -> a
    freeVarErr err =
      error $
        "Free variable found when optimizing: "
          <> show err
          <> "\nIf you see this, report it as a bug."

-- Helpers

fixPrecompiled ::
  NonEmptyVector (ANFBind Analysis) ->
  Set Int ->
  (NonEmptyVector (ANFBind Analysis), Set Int)
fixPrecompiled binds usedNames =
  let precompiledCodeIxes = NEVector.findIndices isPrecompiled binds
      (rewritten, usedNames') = runState (Vector.mapM checkAndRewrite precompiledCodeIxes) usedNames
   in (NEVector.unsafeUpdate binds rewritten, usedNames')
  where
    isPrecompiled :: ANFBind Analysis -> Bool
    isPrecompiled = \case
      ANFLeaf (LCompiled _ _) -> True
      _ -> False
    checkAndRewrite :: Int -> State (Set Int) (Int, ANFBind Analysis)
    checkAndRewrite ix = case binds NEVector.! ix of
      ANFLeaf (LCompiled ann code) -> do
        currentUsed <- get
        let (code', used') = rewriteUniques code currentUsed
        put used'
        pure (ix, ANFLeaf (LCompiled ann code'))
      _ ->
        error $
          "When rewriting precompiled code, we were directed to rewrite something else.\n"
            <> "If you see this, report a bug."

findIdentity :: NonEmptyVector (ANFBind Analysis) -> Maybe Id
findIdentity binds = Id <$> NEVector.findIndex go binds
  where
    go :: ANFBind Analysis -> Bool
    go = \case
      ANFLam _ bvs (AVar h') -> case NEVector.uncons bvs of
        (Just (BoundVar h _), rest) -> Vector.null rest && h == h'
        _ -> False
      _ -> False

-- Check every bind to see if it's a fixpoint, and if it is, record its
-- position.
--
-- We need this for two reasons:
--
-- 1. To know if we have any fixed points at all; and
-- 2. How many we have, so that we can apply the fixpoint transform safely.
doFixpointAnalysis ::
  forall (ann :: Type).
  NonEmptyVector (ANFBind ann) -> Set Int
doFixpointAnalysis = NEVector.ifoldl' go Set.empty
  where
    go :: Set Int -> Int -> ANFBind ann -> Set Int
    go acc pos = \case
      ANFFix {} -> Set.insert pos acc
      _ -> acc

mkFixpointName ::
  Map Int PLC.Name ->
  Int ->
  RWS (Set Int) () Int (Map Int PLC.Name)
mkFixpointName acc i = do
  fresh <- untilM getFresh (asks . Set.notMember)
  let name = mkName "mArg" fresh
  pure . Map.insert i name $ acc

-- Check every bind to see if it's a composition, and if it is, record its
-- position.
--
-- We need this for two reasons:
--
-- 1. To know if there are any compositions at all; and
-- 2. How many we have, so we can apply the composition transform safely.
doCompositionAnalysis ::
  forall (ann :: Type).
  NonEmptyVector (ANFBind ann) -> Set Int
doCompositionAnalysis = NEVector.ifoldl' go Set.empty
  where
    go :: Set Int -> Int -> ANFBind ann -> Set Int
    go acc pos = \case
      ANFCompose {} -> Set.insert pos acc
      _ -> acc

mkCompositionName ::
  Map Int PLC.Name ->
  Int ->
  RWS (Set Int) () Int (Map Int PLC.Name)
mkCompositionName acc i = do
  freshForZ <- untilM getFresh (asks . Set.notMember)
  let name = mkName "compArg" freshForZ
  pure . Map.insert i name $ acc

-- A read-only environment for compilation.
data CompileEnv = CompileEnv
  { -- All ANF binds, with demand analysis, together with their unique names
    ceBinds :: NonEmptyVector (PLC.Name, ANFBind Analysis),
    -- Unique names for each fixpoint we have to compile
    ceFPNameMap :: Map Int PLC.Name,
    -- A unique name for each composition we have to compile
    ceCompNameMap :: Map Int PLC.Name,
    -- A unique name for unused function parameters
    ceUnusedParamName :: PLC.Name,
    -- Whether the identify functions occurs, and if so, where
    ceTheIdentity :: Maybe Id
  }

data CompileState = CompileState
  { -- All previously-compiled binds, by id
    csCache :: Map Id UPLCTerm,
    -- Binds that should be `let`-bound that we've compiled so far
    csToLetBind :: Set Id
  }

newtype CompileM (a :: Type) = CompileM (RWS CompileEnv () CompileState a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader CompileEnv,
      MonadState CompileState
    )
    via (RWS CompileEnv () CompileState)

runCompileM ::
  forall (a :: Type).
  CompileM a ->
  CompileEnv ->
  CompileState ->
  a
runCompileM (CompileM comp) env = fst . evalRWS comp env

untilM ::
  forall (a :: Type) (m :: Type -> Type).
  (Monad m) =>
  m a ->
  (a -> m Bool) ->
  m a
untilM act cond = do
  res <- act
  stop <- cond res
  if stop then pure res else untilM act cond

mkName :: Text -> Int -> PLC.Name
mkName t = PLC.Name t . PLC.Unique

compile ::
  forall (m :: Type -> Type).
  (MonadState CompileState m, MonadReader CompileEnv m) =>
  m UPLCTerm
compile = do
  binds <- asks (NEVector.map snd . ceBinds)
  NEVector.imapM_ compileWithCache binds
  let topBindIx = NEVector.length binds - 1
  -- This cannot 'miss', as we have compiled every bind, and the top bind is
  -- always the last bind.
  fromJust <$> gets (Map.lookup (Id topBindIx) . csCache)
  where
    compileWithCache :: Int -> ANFBind Analysis -> m ()
    compileWithCache bindIx bind = do
      compiledCode <- compileBind bindIx bind
      let (Analysis shouldBind whatToBind) = getANFBindAnn bind
      withLetBinds <- foldrM doLetBind compiledCode . Map.toAscList $ whatToBind
      let ourId = Id bindIx
      modify (cacheCode shouldBind ourId withLetBinds)
    cacheCode :: BindingStatus -> Id -> UPLCTerm -> CompileState -> CompileState
    cacheCode shouldBind i code (CompileState cache whatToLet) =
      let cache' = Map.insert i code cache
       in CompileState cache' $ case shouldBind of
            ShouldBind -> Set.insert i whatToLet
            Shouldn'tBind -> whatToLet
    doLetBind :: (Rank, NESet Id) -> UPLCTerm -> m UPLCTerm
    doLetBind (_, seqBinds) acc = do
      withCode <- traverse (\i -> (,) <$> getBindName i <*> getCode i) . NESet.toList $ seqBinds
      pure $
        if NESet.size seqBinds < parallelBindThreshold
          then foldr (uncurry uplcLet) acc withCode
          else uplcLetP (NEVector.fromNonEmpty withCode) acc
    -- Set on the basis that `n` sequential binds require `n` lambdas and `n`
    -- applications, while `n` parallel binds require 1 case, 1 constr and `n`
    -- lambdas. This gives parity at 2, but may need adjustment later.
    parallelBindThreshold :: Int
    parallelBindThreshold = 2
    -- This is safe because we never request something we haven't yet compiled.
    getCode :: Id -> m UPLCTerm
    getCode i = gets (fromJust . Map.lookup i . csCache)
    getBindName :: Id -> m UPLC.Name
    getBindName (Id i) = asks (\ce -> fst $ ceBinds ce NEVector.! i)

compileBind ::
  forall (m :: Type -> Type).
  (MonadState CompileState m, MonadReader CompileEnv m) =>
  Int ->
  ANFBind Analysis ->
  m UPLCTerm
compileBind bindIx = \case
  -- Leaves can never reference anything, so no namechecking needed
  ANFLeaf ell -> pure $ case ell of
    LConstant _ c -> uplcConstant c
    LBuiltin _ f -> uplcBuiltin f
    LCompiled _ code -> code
    LError _ -> uplcError
  ANFForce _ body -> uplcForce <$> checkCache body
  ANFDelay _ body -> uplcDelay <$> checkCache body
  -- An `ANFFix` node contains two (relevant) pieces of information:
  --
  -- \* A name for the `self` argument; and
  -- \* A lambda-like body using the `self` argument.
  --
  -- Let this body be `F`, and the named variable be `self`. In order to compile
  -- the fixed point, we must do the following steps:
  --
  -- 1. Perform the substitution `F{self -> self self}`; namely, replace every
  --    use of `self` with an application of `self` to `self`. Name this `F'`.
  -- 2. Transform `F'` into `\self -> F'`.
  -- 3. Compile the `M` combinator (\x -> x x). To do this, we have a variable
  --    specifically set aside for each fixed point in the ANF.
  -- 4. Construct (and compile) `M (\self -> F')`.
  --
  -- As `M` is quite small, inlining it at every use site is much cheaper than
  -- trying to `let`-bind it. Furthermore, inlining `M` everywhere is safe, as
  -- it is a closed subcomputation, and if `F` is unique up to alpha
  -- equivalence, so is `F'`, `\self -> F'` and `M (\self -> F')` by the same
  -- reasoning.
  ANFFix _ bv body -> do
    bodyCode <- checkCache body
    -- We have to translate F to a lambda before doing the rest. This also
    -- requires us to rewrite every use of the named variable in `bodyCode` to a
    -- self-application of the named variable instead.
    let fVarName = selfToName bv
    let selfApp = uplcApply1 (uplcVar fVarName) (uplcVar fVarName)
    let subbedBody = substituteVarFor fVarName selfApp bodyCode
    -- `F' = F{self -> self self}`
    let bodyLam = uplcLam1 fVarName subbedBody
    -- This cannot 'miss' as we checked before for all fixpoint sites and made a
    -- name for each.
    mArgName <- asks (fromJust . Map.lookup bindIx . ceFPNameMap)
    -- `M = \x -> x x`, using the reserved name
    let m = uplcMCombinator mArgName
    -- Assemble everything
    pure . uplcApply1 m $ bodyLam
  ANFConstr _ tag fields -> uplcConstr tag <$> traverse checkCache fields
  ANFCase _ scrut handlers -> uplcCase <$> checkCache scrut <*> traverse checkCache handlers
  ANFApply _ f xs -> do
    -- Check if `f` is the identity
    applyingToId <- isTheIdentity f
    xsCode <- traverse checkCache xs
    -- If `f` is the identity, we already know for certain that there's only one
    -- argument. Thus, we can just compile that instead.
    if applyingToId
      then pure . NEVector.head $ xsCode
      else do
        fCode <- checkCache f
        let xsLen = NEVector.length xsCode
        if xsLen <= 2
          -- More efficient to compile a chain of `app`.
          then pure . uplcApply fCode $ xsCode
          -- More efficient to 'pack' `xs` into a `constr`, then `case` on it
          -- immediately with `f` as the sole handler
          else do
            let constrCall = uplcConstr 0 . NEVector.toVector $ xsCode
            let soleHandler = NEVector.singleton fCode
            pure . uplcCase constrCall $ soleHandler
  ANFLam _ bvs body -> do
    bodyCode <- checkCache body
    asBvNames <- NEVector.mapM bvToName bvs
    pure . uplcLam asBvNames $ bodyCode
  -- For compositions, given components `[f_1, f_2, ... , f_k]`, we want to
  -- generate `\z -> f_1 (f_2 ( ... (f_k z) ... )`. We have a name set aside for
  -- `z` to ensure it's unique.
  ANFCompose _ components -> do
    componentCodes <- traverse checkCache components
    -- This cannot 'miss', as we have checked all composition sites and set
    -- aside arguments for them specifically.
    compArgName <- asks (fromJust . Map.lookup bindIx . ceCompNameMap)
    -- We have to fold 'backwards' here, as application order would apply the
    -- _last_ item in the composition first. We could reverse the vector, but
    -- this would require copying it, so we don't.
    let len = NEVector.length componentCodes
    let reverseIxes = [len - 1, len - 2 .. 0]
    let asCompArg = uplcVar compArgName
    let compBody = foldl' (\acc i -> uplcApply1 (componentCodes NEVector.! i) acc) asCompArg reverseIxes
    pure . uplcLam1 compArgName $ compBody
  where
    selfToName :: BoundVar -> PLC.Name
    selfToName (BoundVar (Hash h) _) = mkName "arg" h
    bvToName :: Maybe BoundVar -> m PLC.Name
    bvToName = \case
      -- Unused argument, use reserved name
      Nothing -> asks ceUnusedParamName
      Just (BoundVar (Hash h) _) -> pure . mkName "arg" $ h
    checkCache :: Ref -> m UPLCTerm
    checkCache = \case
      -- Variables always inline, so we don't need to check anything for them.
      AVar (Hash h) -> pure . uplcVar . mkName "arg" $ h
      -- If this identifier is meant to be let-bound later, we have to refer to
      -- it by name. Otherwise, we just retrieve its code and inline it.
      AnId i@(Id ix) -> do
        shouldBind <- gets (Set.member i . csToLetBind)
        if shouldBind
          -- Get its assigned name, wrap it in a variable.
          then do
            assignedName <- asks (\ce -> fst $ ceBinds ce NEVector.! ix)
            pure . uplcVar $ assignedName
          -- Inline its code. This cannot 'miss' due to the compilation order
          -- proceeding from dependencies to dependents.
          else gets (fromJust . Map.lookup i . csCache)
    isTheIdentity :: Ref -> m Bool
    isTheIdentity = \case
      AnId i -> asks ((== Just i) . ceTheIdentity)
      _ -> pure False
