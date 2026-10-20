{-# LANGUAGE TemplateHaskellQuotes #-}

-- | @since 2.0.0
module Plutarch.TH.DataPlutus
  ( deriveDataPlutus,
  )
where

import Control.Monad (when)
import Data.Coerce (coerce)
import Data.Foldable (foldrM)
import Data.Traversable.WithIndex (itraverse)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Language.Haskell.TH
  ( BndrVis,
    Body (NormalB),
    Dec,
    Exp
      ( AppE,
        AppTypeE,
        CaseE,
        ConE,
        LitE,
        VarE
      ),
    Lit (IntegerL, StringL),
    Match (Match),
    Name,
    Pat (ConP, VarP),
    Q,
    TyVarBndr,
    Type,
    nameBase,
    newName,
  )
import Plutarch.Helpers.TH
  ( PType,
    PTypeProduct (PTypeProduct),
    PTypeSum (PTypeSum),
    checkAndMark,
    checkAndMarkUnwrapped,
    fullTypeName,
    mkAnonPLam,
    mkContextOf,
    mkPLam,
    mkUncons,
    pconstrDataE,
    pequalsDataE,
    plam'E,
    pmkConsE,
    pnilDataE,
    punsafeCaseE,
    punsafeCoerceE,
    punsafeConstantE,
    terminateList,
    toSomeTermE,
    unwrapField,
  )
import Plutarch.Primitive.BuiltinFun (pheadList)
import Plutarch.Primitive.CanData (PCanData)
import Plutarch.Primitive.Con (PCon (pcon'))
import Plutarch.Primitive.Debug (PDebug (pdebug))
import Plutarch.Primitive.Eq (PEq (peq))
import Plutarch.Primitive.Match (PMatch (pmatch'))
import Plutarch.Primitive.Representation (PlutarchType (PRepresentation))
import Plutarch.Primitive.Singular (PData, PString)
import Plutarch.Primitive.Validate (PValidateData (pwithValidated))
import PlutusCore qualified as PLC

-- | @since wip
deriveDataPlutus ::
  Vector (TyVarBndr BndrVis) ->
  Name ->
  PTypeSum ->
  Bool ->
  Q [Dec]
deriveDataPlutus tvbs name (PTypeSum typeStructure) deriveEqDebug = case NEVector.unsnoc typeStructure of
  (ps, (nameLast, PTypeProduct fieldsLast)) -> do
    when (Vector.null ps) (fail "Use the DataList strategy for a Data-encoded type with a single constructor.")
    when
      (NEVector.all (Vector.null . coerce @_ @(Vector PType) . snd) typeStructure)
      (fail "Use the Enum strategy for types with no fields in any 'arm.")
    unwrappedFieldsLast <- traverse unwrapField fieldsLast
    unwrappedPS <- traverse (\(name', PTypeProduct fields) -> (name',) <$> traverse unwrapField fields) ps
    let allConFields = NEVector.snocV unwrappedPS (nameLast, unwrappedFieldsLast)
    markedFieldsLast <- traverse (checkAndMark deriveEqDebug name) fieldsLast
    markedRest <-
      traverse
        ( \(name', PTypeProduct fields) ->
            (name',)
              <$> traverse (checkAndMark deriveEqDebug name) fields
        )
        ps
    let markedAll = NEVector.snocV markedRest (nameLast, markedFieldsLast)
    plutarchTypeDec <- derivePlutarchType tvbs name
    pmatchDec <- derivePMatch tvbs name unwrappedPS (nameLast, unwrappedFieldsLast)
    pconDec <- derivePCon tvbs name allConFields
    peqDec <- if deriveEqDebug then derivePEq tvbs name else pure []
    pdebugDec <- if deriveEqDebug then derivePDebug tvbs name markedAll else pure []
    pcanDataDec <- derivePCanData tvbs name
    let markedNoAsData = fmap (checkAndMarkUnwrapped name) allConFields
    pvalidateDataDec <- derivePValidateData tvbs name markedNoAsData
    pure $
      plutarchTypeDec
        <> pmatchDec
        <> pconDec
        <> peqDec
        <> pdebugDec
        <> pcanDataDec
        <> pvalidateDataDec

-- Helpers

derivePlutarchType :: Vector (TyVarBndr BndrVis) -> Name -> Q [Dec]
derivePlutarchType tyVars tyName =
  [d|
    instance ($ctx) => PlutarchType $name where
      type PRepresentation $name = PData
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf [''PCanData] $ tyVars

derivePMatch ::
  Vector (TyVarBndr BndrVis) ->
  Name ->
  Vector (Name, Vector Type) ->
  (Name, Vector Type) ->
  Q [Dec]
derivePMatch tyVars tyName cs (nameLast, fieldsLast) =
  [d|
    instance ($ctx) => PMatch $name where
      pmatch' x f = $punsafeCaseE x $(mkMatches 'f)
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf [''PCanData] $ tyVars
    mkMatches :: Name -> Q Exp
    mkMatches contName = do
      let arityLast = Vector.length fieldsLast
      let namesRest = fmap fst cs
      let aritiesRest = fmap (Vector.length . snd) cs
      handlerLast <- mkHandler contName (nameLast, arityLast)
      handlersRest <- traverse (mkHandler contName) (Vector.zip namesRest aritiesRest)
      start <- [e|NEVector.singleton ($toSomeTermE $(pure handlerLast))|]
      foldrM (\e acc -> [e|NEVector.cons ($toSomeTermE $(pure e)) $(pure acc)|]) start handlersRest
    mkHandler :: Name -> (Name, Int) -> Q Exp
    mkHandler contName (conName, arity) = case arity of
      0 -> mkAnonPLam . pure . AppE (VarE contName) . ConE $ conName
      _ -> mkPLam $ \fieldsName -> do
        let fieldsE = VarE fieldsName
        go contName conName fieldsE [] arity
    go :: Name -> Name -> Exp -> [Name] -> Int -> Q Exp
    go contName conName lastTail fieldNamesBackwards = \case
      -- Reverse field names, take last head, assemble.
      1 -> do
        let fieldNames = reverse fieldNamesBackwards
        conAppButLast <- foldrM (\headName acc -> AppE acc <$> [e|$punsafeCoerceE $(pure (VarE headName))|]) (ConE conName) fieldNames
        conAppE <- AppE conAppButLast <$> [e|$punsafeCoerceE (pheadList @PData # $(pure lastTail))|]
        pure . AppE (VarE contName) $ conAppE
      -- Take one more field
      n -> mkUncons lastTail $ \headName tailName ->
        go contName conName (VarE tailName) (headName : fieldNamesBackwards) (n - 1)

derivePCon ::
  Vector (TyVarBndr BndrVis) ->
  Name ->
  NonEmptyVector (Name, Vector Type) ->
  Q [Dec]
derivePCon tyVars tyName constructors =
  [d|
    instance ($ctx) => PCon $name where
      pcon' x = $(matches 'x)
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf [''PCanData] $ tyVars
    matches :: Name -> Q Exp
    matches bindName = CaseE (VarE bindName) . NEVector.toList <$> NEVector.imapM mkMatch constructors
    mkMatch :: Int -> (Name, Vector Type) -> Q Match
    mkMatch conIx (conName, conFields) = do
      fieldNames <- itraverse (\i _ -> newName $ "f" <> show i) conFields
      let conMatchPat = ConP conName [] . Vector.toList . fmap VarP $ fieldNames
      let constrIx = LitE . IntegerL . fromIntegral $ conIx
      constrE <- [e|$punsafeConstantE (PLC.someValue @Integer $(pure constrIx))|]
      start <- pnilDataE
      constrList <- foldrM go start fieldNames
      matchBody <- [e|$pconstrDataE # $(pure constrE) # $(pure constrList)|]
      pure . Match conMatchPat (NormalB matchBody) $ []
    go :: Name -> Exp -> Q Exp
    go fieldName acc = [e|$pmkConsE # pfundamental $(pure . VarE $ fieldName) # $(pure acc)|]

derivePEq :: Vector (TyVarBndr BndrVis) -> Name -> Q [Dec]
derivePEq tyVars tyName =
  [d|
    instance ($ctx) => PEq $name where
      peq = $plam'E $ \x -> $plam'E $ \y -> $pequalsDataE # pfundamental x # pfundamental y
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf [''PCanData] $ tyVars

derivePDebug :: Vector (TyVarBndr BndrVis) -> Name -> NonEmptyVector (Name, Vector (Bool, Type)) -> Q [Dec]
derivePDebug tyVars tyName constructors =
  if NEVector.any (\(_, fields) -> Vector.any fst fields) constructors
    then -- We're isorecursive, so we need a fixpoint.
      [d|
        instance ($ctx) => PDebug $name where
          pdebug t f =
            f
              ( pfix
                  ( \self -> $plam'E $ \x -> pmatch x $ \y ->
                      $(mkMatchesRec 'self 'y)
                  )
                  # t
              )
        |]
    else -- Not isorecursive, so no need.
      [d|
        instance ($ctx) => PDebug $name where
          pdebug t f = f (pmatch t $ \x -> $(mkMatches 'x))
        |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf [''PDebug, ''PCanData] $ tyVars
    mkMatchesRec :: Name -> Name -> Q Exp
    mkMatchesRec selfName bindName =
      CaseE (VarE bindName) . NEVector.toList <$> NEVector.mapM (mkDebugRec selfName) constructors
    mkMatches :: Name -> Q Exp
    mkMatches bindName =
      CaseE (VarE bindName)
        . NEVector.toList
        <$> (NEVector.mapM mkDebug . fmap (fmap (fmap snd)) $ constructors)
    mkDebug :: (Name, Vector Type) -> Q Match
    mkDebug (conName, conFields) = do
      fieldNames <- itraverse (\i _ -> newName $ "f" <> show i) conFields
      let conMatchPat = ConP conName [] . Vector.toList . fmap VarP $ fieldNames
      startLit <- [e|pconstant @PString $(pure . LitE . StringL $ nameBase conName <> " {\n")|]
      endLit <- [e|pconstant @PString "}"|]
      fieldEs <- traverse mkFieldShow fieldNames
      matchBody <- go startLit (Vector.toList fieldEs <> [endLit])
      pure . Match conMatchPat (NormalB matchBody) $ []
    mkFieldShow :: Name -> Q Exp
    mkFieldShow fVarName = do
      commaE <- [e|pconstant @PString ", "|]
      newlineE <- [e|pconstant @PString "\n"|]
      fieldE <- [e|pshow (pfromData $(pure . VarE $ fVarName))|]
      [e|pappendString # $(pure commaE) # (pappendString # $(pure fieldE) # $(pure newlineE))|]
    mkDebugRec :: Name -> (Name, Vector (Bool, Type)) -> Q Match
    mkDebugRec selfName (conName, conFields) = do
      fieldNames <- itraverse (\i _ -> newName $ "f" <> show i) conFields
      let conMatchPat = ConP conName [] . Vector.toList . fmap VarP $ fieldNames
      startLit <- [e|pconstant @PString $(pure . LitE . StringL $ nameBase conName <> " {\n")|]
      endLit <- [e|pconstant @PString "}"|]
      fieldEs <-
        traverse
          (mkFieldShowRec selfName)
          (Vector.zipWith (\n (isRec, _) -> (n, isRec)) fieldNames conFields)
      matchBody <- go startLit (Vector.toList fieldEs <> [endLit])
      pure . Match conMatchPat (NormalB matchBody) $ []
    mkFieldShowRec :: Name -> (Name, Bool) -> Q Exp
    mkFieldShowRec selfName (fVarName, isRec) = do
      commaE <- [e|pconstant @PString ", "|]
      newlineE <- [e|pconstant @PString "\n"|]
      fieldE <-
        if isRec
          then [e|$(pure . VarE $ selfName) # pfromData $(pure . VarE $ fVarName)|]
          else [e|pshow $(pure . VarE $ fVarName)|]
      [e|pappendString # $(pure commaE) # (pappendString # $(pure fieldE) # $(pure newlineE))|]
    go :: Exp -> [Exp] -> Q Exp
    go acc = \case
      [] -> pure acc
      (e : es) -> [e|pappendString # $(pure acc) # $(go e es)|]

derivePCanData :: Vector (TyVarBndr BndrVis) -> Name -> Q [Dec]
derivePCanData tyVars tyName =
  [d|
    instance ($ctx) => PCanData $name
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf [''PCanData] $ tyVars

derivePValidateData ::
  Vector (TyVarBndr BndrVis) ->
  Name ->
  NonEmptyVector (Vector (Bool, Type)) ->
  Q [Dec]
derivePValidateData tyVars tyName cs =
  if NEVector.any (Vector.any fst) cs
    then -- We're isorecursive, so we need a fixpoint. This has to be done in
    -- a somewhat strange way, as we can only take fixpoints of Plutus
    -- arrows, not Haskell ones, so we have to wrap the entire process into
    -- a Plutus arrow, then apply the arguments of the type class method to
    -- it.
      [d|
        instance ($ctx) => PValidateData $name where
          pwithValidated d res =
            pfix
              ( \self -> $plam'E $ \d' ->
                  $plam'E $ \resInner -> $punsafeCaseE d' $ $(mkCasesRec 'self 'resInner)
              )
              # d
              # res
        |]
    else -- Not isorecursive.
      [d|
        instance ($ctx) => PValidateData $name where
          pwithValidated d res = $punsafeCaseE d $ $(mkCases 'res)
        |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf [''PValidateData] $ tyVars
    mkCases :: Name -> Q Exp
    mkCases contName = do
      let arities = fmap Vector.length cs
      let withArities = NEVector.zip arities cs
      let (restCs, lastC) = NEVector.unsnoc withArities
      handlerLast <- mkHandler contName 'pwithValidated lastC
      handlersRest <- traverse (mkHandler contName 'pwithValidated) restCs
      start <- [e|NEVector.singleton ($toSomeTermE $(pure handlerLast))|]
      foldrM (\e acc -> [e|NEVector.cons ($toSomeTermE $(pure e)) $(pure acc)|]) start handlersRest
    mkCasesRec :: Name -> Name -> Q Exp
    mkCasesRec selfName contName = do
      let arities = fmap Vector.length cs
      let withArities = NEVector.zip arities cs
      let (restCs, lastC) = NEVector.unsnoc withArities
      handlerLast <- mkHandler contName selfName lastC
      handlersRest <- traverse (mkHandler contName selfName) restCs
      start <- [e|NEVector.singleton ($toSomeTermE $(pure handlerLast))|]
      foldrM (\e acc -> [e|NEVector.cons ($toSomeTermE $(pure e)) $(pure acc)|]) start handlersRest
    mkHandler :: Name -> Name -> (Int, Vector (Bool, Type)) -> Q Exp
    mkHandler contName recName (arity, tys) = mkPLam $ \fieldsName -> do
      let fieldsE = VarE fieldsName
      go contName recName arity tys fieldsE 0
    go :: Name -> Name -> Int -> Vector (Bool, Type) -> Exp -> Int -> Q Exp
    go contName recName arity tys lastTailE ix
      | ix == arity = terminateList lastTailE contName
      | otherwise = mkUncons lastTailE $ \headName tailName -> do
          restE <- go contName recName arity tys (VarE tailName) (ix + 1)
          let (isRecTy, t) = tys Vector.! ix
          let headE = VarE headName
          if isRecTy
            -- Apply as a Plutus arrow.
            then [e|$(pure . VarE $ recName) # $(pure headE) # ($(pure restE))|]
            else do
              -- Use the `pwithValidated` transform.
              let validateE = AppTypeE (VarE 'pwithValidated) t
              [e|$(pure validateE) $(pure headE) ($(pure restE))|]
