{-# LANGUAGE TemplateHaskellQuotes #-}

-- | @since 2.0.0
module Plutarch.TH.SOP
  ( deriveSOP,
  )
where

import Control.Monad (when)
import Data.Coerce (coerce)
import Data.Foldable (foldl', foldlM, foldrM)
import Data.Traversable.WithIndex (itraverse)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Language.Haskell.TH
  ( BndrVis,
    Body (NormalB),
    Dec,
    Exp (AppE, CaseE, ConE, LamE, LitE, VarE),
    Lit (IntegerL, StringL),
    Match (Match),
    Name,
    Pat (ConP, VarP, WildP),
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
    fullTypeName,
    mkContextOf,
    plam'E,
    punsafeCaseE,
    punsafeConstrE,
    toSomeTermE,
  )
import Plutarch.Primitive.Con (PCon (pcon'))
import Plutarch.Primitive.Debug (PDebug (pdebug))
import Plutarch.Primitive.Eq (PEq (peq))
import Plutarch.Primitive.Match (PMatch (pmatch'))
import Plutarch.Primitive.Representation (PlutarchType (PRepresentation))
import Plutarch.Primitive.Singular (PSOP, PString)
import Plutarch.Primitive.Term (plam')

-- | @since 2.0.0
deriveSOP :: Vector (TyVarBndr BndrVis) -> Name -> PTypeSum -> Q [Dec]
deriveSOP tvbs name (PTypeSum typeStructure) = case NEVector.unsnoc typeStructure of
  (ps, (nameLast, PTypeProduct fieldsLast)) -> do
    when
      (NEVector.all (Vector.null . coerce @_ @(Vector PType) . snd) typeStructure)
      (fail "Use the Enum strategy for types with no fields in any 'arm'.")
    markedFieldsLast <- traverse (checkAndMark name) fieldsLast
    markedRest <- traverse (\(name', PTypeProduct fields) -> (name',) <$> traverse (checkAndMark name) fields) ps
    plutarchTypeDec <- derivePlutarchType tvbs name
    pmatchDec <- derivePMatch tvbs name markedRest (nameLast, markedFieldsLast)
    let markedAll = NEVector.snocV markedRest (nameLast, markedFieldsLast)
    pconDec <- derivePCon tvbs name markedAll
    peqDec <- derivePEq tvbs name markedAll
    pdebugDec <- derivePDebug tvbs name markedAll
    pure $ plutarchTypeDec <> pmatchDec <> pconDec <> peqDec <> pdebugDec

-- Helpers

derivePlutarchType :: Vector (TyVarBndr BndrVis) -> Name -> Q [Dec]
derivePlutarchType tyVars tyName =
  [d|
    instance ($ctx) => PlutarchType $name where
      type PRepresentation $name = PSOP
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf [''PlutarchType] $ tyVars

derivePMatch ::
  Vector (TyVarBndr BndrVis) ->
  Name ->
  Vector (Name, Vector (Bool, Type)) ->
  (Name, Vector (Bool, Type)) ->
  Q [Dec]
derivePMatch tyVars tyName cs (nameLast, fieldsLast) =
  [d|
    instance ($ctx) => PMatch $name where
      pmatch' x f = let handlers = $(mkHandlers 'f) in $punsafeCaseE x handlers
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf [''PlutarchType] $ tyVars
    -- We need one handler for each 'arm', of appropriate arity for the field
    -- counts.
    --
    -- We know there's at least one arm because we checked before we made it
    -- here.
    mkHandlers :: Name -> Q Exp
    mkHandlers contName = do
      let arityLast = Vector.length fieldsLast
      let namesRest = fmap fst cs
      let aritiesRest = fmap (Vector.length . snd) cs
      handlerLast <- mkHandler contName (nameLast, arityLast)
      handlersRest <- traverse (mkHandler contName) (Vector.zip namesRest aritiesRest)
      start <- [e|NEVector.singleton ($toSomeTermE $(pure handlerLast))|]
      foldrM (\e acc -> [e|NEVector.cons ($toSomeTermE $(pure e)) $(pure acc)|]) start handlersRest
    mkHandler :: Name -> (Name, Int) -> Q Exp
    mkHandler contName (conName, arity) = do
      argNames <- case arity of
        0 -> pure []
        n -> traverse (\i -> newName $ "x" <> show i) [0, 1 .. n - 1]
      let conCallExp = AppE (VarE contName) . foldl' (\acc -> AppE acc . VarE) (ConE conName) $ argNames
      pure . foldr (\name' -> AppE (VarE 'plam') . LamE [VarP name']) conCallExp $ argNames

derivePCon ::
  Vector (TyVarBndr BndrVis) ->
  Name ->
  NonEmptyVector (Name, Vector (Bool, Type)) ->
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
    ctx = pure . mkContextOf [''PlutarchType] $ tyVars
    matches :: Name -> Q Exp
    matches bindName = CaseE (VarE bindName) . NEVector.toList <$> NEVector.imapM mkMatch constructors
    mkMatch :: Int -> (Name, Vector (Bool, Type)) -> Q Match
    mkMatch conIx (conName, conFields) = do
      fieldNames <- itraverse (\i _ -> newName $ "f" <> show i) conFields
      let conMatchPat = ConP conName [] . Vector.toList . fmap VarP $ fieldNames
      let constrIx = LitE . IntegerL . fromIntegral $ conIx
      constrVec <- foldrM (\n acc -> [e|Vector.cons ($toSomeTermE $(pure (VarE n))) $(pure acc)|]) (VarE 'Vector.empty) fieldNames
      matchBody <- [e|$punsafeConstrE $(pure constrIx) $(pure constrVec)|]
      pure . Match conMatchPat (NormalB matchBody) $ []

derivePEq ::
  Vector (TyVarBndr BndrVis) ->
  Name ->
  NonEmptyVector (Name, Vector (Bool, Type)) ->
  Q [Dec]
derivePEq tyVars tyName constructors =
  if NEVector.any (\(_, fields) -> Vector.any fst fields) constructors
    -- If we're isorecursive, we have to make sure we have a fixpoint with a
    -- `self` available for the recursive fields.
    then
      [d|
        instance ($ctx) => PEq $name where
          peq = pfix $ \self -> $plam'E $ \x -> $plam'E $ \y ->
            pmatch x $ \xInner ->
              pmatch y $ \yInner ->
                $(peqImpl 'self 'xInner 'yInner)
        |]
    -- For uniformity, we use _one_ generator, and just pass `peq` as the `self`
    -- in cases where the type is 'flat'.
    else
      [d|
        instance ($ctx) => PEq $name where
          peq = $plam'E $ \x -> $plam'E $ \y ->
            pmatch x $ \xInner ->
              pmatch y $ \yInner ->
                $(peqImpl 'peq 'xInner 'yInner)
        |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf [''PEq] $ tyVars
    peqImpl :: Name -> Name -> Name -> Q Exp
    peqImpl selfName xName yName = do
      matches <- NEVector.toList <$> traverse (mkMatch selfName yName) constructors
      pure . CaseE (VarE xName) $ matches
    mkMatch :: Name -> Name -> (Name, Vector (Bool, Type)) -> Q Match
    mkMatch selfName yName (conName, conFields) = do
      fieldNames <-
        itraverse
          ( \i e ->
              (,,)
                <$> newName ("x" <> show i)
                <*> newName ("y" <> show i)
                <*> pure e
          )
          conFields
      let xMatchPat = ConP conName [] . Vector.toList . fmap (\(x, _, _) -> VarP x) $ fieldNames
      let yMatchPat = ConP conName [] . Vector.toList . fmap (\(_, y, _) -> VarP y) $ fieldNames
      hitExp <- case Vector.uncons fieldNames of
        Nothing -> [e|ptrue|]
        Just ((xField, yField, (isRec, _)), fields) -> do
          let xVar = VarE xField
          let yVar = VarE yField
          start <-
            if isRec
              then [e|$(pure (VarE selfName)) # $(pure xVar) # $(pure yVar)|]
              else [e|peq # $(pure xVar) # $(pure yVar)|]
          foldlM (mkPand selfName) start fields
      missExp <- [e|pfalse|]
      let matchBody =
            CaseE
              (VarE yName)
              [ Match yMatchPat (NormalB hitExp) [],
                Match WildP (NormalB missExp) []
              ]
      pure . Match xMatchPat (NormalB matchBody) $ []
    mkPand :: Name -> Exp -> (Name, Name, (Bool, Type)) -> Q Exp
    mkPand selfName acc (xName, yName, (isRec, _)) = do
      let xVar = VarE xName
      let yVar = VarE yName
      if isRec
        then [e|pand ($(pure (VarE selfName)) # $(pure xVar) # $(pure yVar)) $(pure acc)|]
        else [e|pand (peq # $(pure xVar) # $(pure yVar)) $(pure acc)|]

derivePDebug ::
  Vector (TyVarBndr BndrVis) ->
  Name ->
  NonEmptyVector (Name, Vector (Bool, Type)) ->
  Q [Dec]
derivePDebug tyVars tyName constructors =
  if NEVector.any (\(_, fields) -> Vector.any fst fields) constructors
    then -- We're isorecursive, so we need a fixpoint.
      [d|
        instance ($ctx) => PDebug $name where
          pdebug t f =
            f
              ( pfix
                  ( \self -> plam' $ \x -> pmatch x $ \y ->
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
    ctx = pure . mkContextOf [''PDebug] $ tyVars
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
      fieldE <- [e|pshow $(pure . VarE $ fVarName)|]
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
          then [e|$(pure . VarE $ selfName) # $(pure . VarE $ fVarName)|]
          else [e|pshow $(pure . VarE $ fVarName)|]
      [e|pappendString # $(pure commaE) # (pappendString # $(pure fieldE) # $(pure newlineE))|]
    go :: Exp -> [Exp] -> Q Exp
    go acc = \case
      [] -> pure acc
      (e : es) -> [e|pappendString # $(pure acc) # $(go e es)|]
