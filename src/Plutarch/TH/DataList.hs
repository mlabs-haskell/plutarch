{-# LANGUAGE TemplateHaskellQuotes #-}

-- | @since 2.0.0
module Plutarch.TH.DataList
  ( deriveDataList,
  )
where

import Control.Monad (unless, when)
import Data.Foldable (foldrM)
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
    Lit (StringL),
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
  ( PTypeProduct (PTypeProduct),
    PTypeSum (PTypeSum),
    fullTypeName,
    isTypeRecursive,
    mkContextOf,
    mkPLam,
    mkUncons,
    pmkConsE,
    pnilDataE,
    punListDataE,
    punsafeCoerceE,
    unwrapField,
  )
import Plutarch.Primitive.BuiltinFun (pheadList)
import Plutarch.Primitive.CanData (PCanData)
import Plutarch.Primitive.Composite (PBList (PBCons, PBNil))
import Plutarch.Primitive.Con (PCon (pcon'))
import Plutarch.Primitive.Debug (PDebug (pdebug))
import Plutarch.Primitive.Match (PMatch (pmatch'))
import Plutarch.Primitive.Representation (PlutarchType (PRepresentation))
import Plutarch.Primitive.Singular (PData, PString)
import Plutarch.Primitive.Validate (PValidateData (pwithValidated))

-- | @since 2.0.0
deriveDataList ::
  Vector (TyVarBndr BndrVis) ->
  Name ->
  PTypeSum ->
  Bool ->
  Q [Dec]
deriveDataList tvbs name (PTypeSum typeStructure) deriveEqDebug = case NEVector.unsnoc typeStructure of
  (ps, (conName, PTypeProduct conStructure)) -> do
    unless (Vector.null ps) (fail "Cannot use DataList strategy for a type with multiple data constructors.")
    case NEVector.fromVector conStructure of
      Nothing -> fail "Use the Enum strategy for types with no fields in any 'arm'."
      Just conStructure' -> do
        when
          (NEVector.any (isTypeRecursive name) conStructure')
          (fail "An isorecursive type cannot use the DataList strategy.")
        unwrapped <- traverse unwrapField conStructure'
        plutarchTypeDec <- derivePlutarchType tvbs name
        pmatchDec <- derivePMatch tvbs name conName unwrapped
        pconDec <- derivePCon tvbs name conName unwrapped
        pdebugDec <- if deriveEqDebug then derivePDebug tvbs name conName unwrapped else pure []
        pcanDataDec <- derivePCanDataDec tvbs name
        pvalidateDataDec <- derivePValidateDataDec tvbs name unwrapped
        pure $
          plutarchTypeDec
            <> pmatchDec
            <> pconDec
            <> pdebugDec
            <> pcanDataDec
            <> pvalidateDataDec

-- Helpers

derivePlutarchType :: Vector (TyVarBndr BndrVis) -> Name -> Q [Dec]
derivePlutarchType tyVars tyName =
  [d|
    instance ($ctx) => PlutarchType $name where
      type PRepresentation $name = PBList PData
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf [''PCanData] $ tyVars

derivePMatch :: Vector (TyVarBndr BndrVis) -> Name -> Name -> NonEmptyVector Type -> Q [Dec]
derivePMatch tyVars tyName cName fieldTypes =
  [d|
    instance ($ctx) => PMatch $name where
      pmatch' x f = $(mkMatchBody 'x 'f)
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf [''PCanData] $ tyVars
    -- We have to do this in such a convoluted way because the continuation (`f`
    -- argument to `pmatch'`) has to be placed on the _inside_ of all our list
    -- unconses. However, at the same time, we _also_ have to build up a large
    -- application chain to our constructor `C`.
    --
    -- We can assume that there is at least one field: this is verified by the
    -- non-emptiness of `fieldTypes`.
    mkMatchBody :: Name -> Name -> Q Exp
    mkMatchBody listName contName =
      go contName cName [] listName (NEVector.length fieldTypes - 1)
    go :: Name -> Name -> [Name] -> Name -> Int -> Q Exp
    go contName cName' headsNamesBackwards lastTailName = \case
      0 -> do
        -- We accumulate the heads needed in reverse order, because otherwise,
        -- this is a quadratic procedure. We can reverse in linear time.
        let headsNames = reverse headsNamesBackwards
        -- Build up applications of all heads to the constructor.
        conAppButLast <- foldrM (\headName acc -> AppE acc <$> [e|$punsafeCoerceE $(pure (VarE headName))|]) (ConE cName') headsNames
        -- Add the last argument by taking the head of the last tail.
        conAppE <- AppE conAppButLast <$> [e|$punsafeCoerceE (pheadList @PData # $(pure (VarE lastTailName)))|]
        -- Hit it with the continuation internally.
        pure . AppE (VarE contName) $ conAppE
      n -> mkUncons lastTailName $ \headName tailName ->
        -- We accumulate the needed names of all the heads we have to take
        -- so that we can apply them to the constructor 'all at once' at
        -- the end.
        go contName cName' (headName : headsNamesBackwards) tailName (n - 1)

derivePCon :: Vector (TyVarBndr BndrVis) -> Name -> Name -> NonEmptyVector Type -> Q [Dec]
derivePCon tyVars tyName cName fieldTypes =
  [d|
    instance ($ctx) => PCon $name where
      pcon' x = $(match 'x)
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf [''PCanData] $ tyVars
    match :: Name -> Q Exp
    match bindName = CaseE (VarE bindName) . (: []) <$> mkMatch
    mkMatch :: Q Match
    mkMatch = do
      fieldNames <- NEVector.imapM (\i _ -> newName $ "f" <> show i) fieldTypes
      let conMatchPat = ConP cName [] . fmap VarP . NEVector.toList $ fieldNames
      start <- pnilDataE
      constrList <- foldrM go start fieldNames
      pure . Match conMatchPat (NormalB constrList) $ []
    go :: Name -> Exp -> Q Exp
    go fieldName acc = [e|$pmkConsE # pfundamental $(pure . VarE $ fieldName) # $(pure acc)|]

derivePDebug :: Vector (TyVarBndr BndrVis) -> Name -> Name -> NonEmptyVector Type -> Q [Dec]
derivePDebug tyVars tyName cName fieldTypes =
  [d|
    instance ($ctx) => PDebug $name where
      pdebug t f = f (pmatch t $ \x -> $(mkMatch 'x))
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf [''PCanData, ''PDebug] $ tyVars
    mkMatch :: Name -> Q Exp
    mkMatch bindName = do
      fieldNames <- NEVector.imapM (\i _ -> newName $ "f" <> show i) fieldTypes
      let conMatchPat = ConP cName [] . fmap VarP . NEVector.toList $ fieldNames
      startLit <- [e|pconstant @PString $(pure . LitE . StringL $ nameBase cName <> " {\n")|]
      endLit <- [e|pconstant @PString "}"|]
      fieldEs <- traverse mkFieldShow fieldNames
      matchBody <- go startLit (NEVector.toList fieldEs <> [endLit])
      pure . CaseE (VarE bindName) $ [Match conMatchPat (NormalB matchBody) []]
    mkFieldShow :: Name -> Q Exp
    mkFieldShow fVarName = do
      commaE <- [e|pconstant @PString ", "|]
      newlineE <- [e|pconstant @PString "\n"|]
      fieldE <- [e|pshow (pfromData $(pure . VarE $ fVarName))|]
      [e|pappendString # $(pure commaE) # (pappendString # $(pure fieldE) # $(pure newlineE))|]
    go :: Exp -> [Exp] -> Q Exp
    go acc = \case
      [] -> pure acc
      (e : es) -> [e|pappendString # $(pure acc) # $(go e es)|]

derivePCanDataDec :: Vector (TyVarBndr BndrVis) -> Name -> Q [Dec]
derivePCanDataDec tyVars tyName =
  [d|
    instance ($ctx) => PCanData $name
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf [''PCanData] $ tyVars

derivePValidateDataDec :: Vector (TyVarBndr BndrVis) -> Name -> NonEmptyVector Type -> Q [Dec]
derivePValidateDataDec tyVars tyName fieldTypes =
  [d|
    instance ($ctx) => PValidateData $name where
      pwithValidated d res = $(mkPlams 'd 'res)
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf [''PValidateData] $ tyVars
    mkPlams :: Name -> Name -> Q Exp
    mkPlams dataVarName contName = do
      unpackE <- [e|$(punListDataE) # $(pure . VarE $ dataVarName)|]
      go contName unpackE 0
    go :: Name -> Exp -> Int -> Q Exp
    go contName acc fieldIx
      -- We're at the end, so check there's no items left
      | fieldIx == NEVector.length fieldTypes = [e|pmatch $(pure acc) $ \case PBNil -> $(pure . VarE $ contName); PBCons _ _ -> perror|]
      | otherwise = do
          startE <- [e|pheadTailList $(pure acc)|]
          innerE <- mkPLam $ \headName -> mkPLam $ \tailName -> do
            let validateE = AppTypeE (VarE 'pwithValidated) (fieldTypes NEVector.! fieldIx)
            rec <- go contName (VarE tailName) (fieldIx + 1)
            [e|$(pure validateE) $(pure . VarE $ headName) ($(pure rec))|]
          [e|$(pure startE) ($(pure innerE))|]
