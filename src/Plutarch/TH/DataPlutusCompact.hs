{-# LANGUAGE TemplateHaskellQuotes #-}

-- | @since 2.0.0
module Plutarch.TH.DataPlutusCompact
  ( deriveDataPlutusCompact,
  )
where

import Control.Monad (when)
import Data.Coerce (coerce)
import Data.Foldable (foldrM)
import Data.Ord (comparing)
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
    fullTypeName,
    mkContextOf,
    mkUncons,
    pconstrDataE,
    pequalsDataE,
    plam'E,
    pmkConsE,
    pnilDataE,
    punConstrDataE,
    punsafeCaseE,
    punsafeCoerceE,
    punsafeConstantE,
    terminateList,
    toSomeTermE,
    unwrapField,
  )
import Plutarch.Primitive.BuiltinFun (pheadList)
import Plutarch.Primitive.CanData (PCanData)
import Plutarch.Primitive.Composite
  ( -- PBList (PBCons, PBNil),
    PBPair (PBPair),
  )
import Plutarch.Primitive.Con (PCon (pcon'))
import Plutarch.Primitive.Debug (PDebug (pdebug))
import Plutarch.Primitive.Eq (PEq (peq))
import Plutarch.Primitive.Match (PMatch (pmatch'))
import Plutarch.Primitive.Representation (PlutarchType (PRepresentation))
import Plutarch.Primitive.Singular (PData, PString)
import Plutarch.Primitive.Validate (PValidateData (pwithValidated))
import Plutarch.TH.DataPlutus (deriveDataPlutus)
import PlutusCore qualified as PLC

-- | @since wip
deriveDataPlutusCompact ::
  Vector (TyVarBndr BndrVis) ->
  Name ->
  PTypeSum ->
  Bool ->
  Q [Dec]
deriveDataPlutusCompact tvbs name t@(PTypeSum typeStructure) deriveEqDebug = do
  if NEVector.any (isEmptyProduct . snd) typeStructure
    then deriveDataPlutus tvbs name t deriveEqDebug
    else case NEVector.unsnoc typeStructure of
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
        pvalidateDataDec <- derivePValidateData tvbs name allConFields
        pure $
          plutarchTypeDec
            <> pmatchDec
            <> pconDec
            <> peqDec
            <> pdebugDec
            <> pcanDataDec
            <> pvalidateDataDec

-- Helpers

isEmptyProduct :: PTypeProduct -> Bool
isEmptyProduct (PTypeProduct p) = Vector.null p

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
      pmatch' x f = pmatch ($punConstrDataE # x) $ \(PBPair tag fields) ->
        $(mkMatchBody 'f 'tag 'fields)
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf [''PCanData] $ tyVars
    mkMatchBody :: Name -> Name -> Name -> Q Exp
    mkMatchBody contName tagName fieldsName = do
      let arityLast = Vector.length fieldsLast
      let namesRest = fmap fst cs
      let aritiesRest = fmap (Vector.length . snd) cs
      -- If we have a minimum required number of fields, we extract them _now_,
      -- to avoid having repeated code in our handlers doing the same thing.
      let minArity = Vector.foldl' min arityLast aritiesRest
      -- Offset all arities by the number we 'preload'
      let arityLast' = arityLast - minArity
      let aritiesRest' = fmap (\x -> x - minArity) aritiesRest
      withPreload contName tagName (arityLast', nameLast) (aritiesRest', namesRest) [] fieldsName minArity
    -- Consider the following data type:
    --
    -- ```
    -- data PThese a b s =
    --     PThis (Term s (PAsData a)) |
    --     PThat (Term s (PAsData b)) |
    --     PThese (Term s (PAsData a)) (Term s (PAsData b))
    -- ```
    --
    -- The most straightforward @Data@ encoding would do something like the
    -- following:
    --
    -- \* `PThis t` becomes `Constr 0 [t]`
    -- \* `PThat t` becomes `Constr 1 [t]`
    -- \* `PThese t1 t2` becomes `Constr 2 [t1, t2]`
    --
    -- To 'take apart' such an encoding, we can see that we _must_ 'pull out'
    -- the first element of the 'field list' no matter which branch we take.
    -- Thus, an efficient sequence of events would be:
    --
    -- 1. Transform `PData` into `(PInteger, PBList PData)`
    -- 2. Take the head of the `PBList PData`
    -- 3. Branch on the tag; if we have `0` or `1`, assemble the `PThese`, if
    --    not, take the head _again_, then assemble the `PThese`.
    --
    -- However, if we were to just follow the constructor logic blindly, we
    -- would _instead_ get:
    --
    -- 1. Transform `PData` into `(PInteger, PBList PData)`
    -- 2. Branch on the tag; if we have `0`, take the head, then assemble, if we
    --    have `1`, take the head, then assemble, if we have 2, take the head
    --    twice, then assemble.
    --
    -- This forces us to duplicate the 'take the head' code into _every_ branch.
    --
    -- `withPreload` essentially ensures that in situations like the one above,
    -- the code that gets generated follows the efficient sequence of events
    -- above. More precisely, if _all_ 'arms' of a data type have a minimal
    -- number of fields, `withPreload` generates code to extract that number of
    -- fields _before_ branching on the tag.
    withPreload ::
      Name ->
      Name ->
      (Int, Name) ->
      (Vector Int, Vector Name) ->
      [Name] ->
      Name ->
      Int ->
      Q Exp
    withPreload contName tagName (arityLast, nameLast') (aritiesRest, namesRest) preloadNamesBackwards lastTail = \case
      0 -> do
        handlerLast <- mkHandler lastTail contName preloadNamesBackwards (nameLast', arityLast)
        handlersRest <- traverse (mkHandler lastTail contName preloadNamesBackwards) (Vector.zip namesRest aritiesRest)
        start <- [e|NEVector.singleton ($toSomeTermE $(pure handlerLast))|]
        handlers <- foldrM (\e acc -> [e|NEVector.cons ($toSomeTermE $(pure e)) $(pure acc)|]) start handlersRest
        [e|$punsafeCaseE $(pure (VarE tagName)) $(pure handlers)|]
      n -> mkUncons (VarE lastTail) $ \headName tailName ->
        -- We accumulate all preloaded names so that we can apply them to the
        -- appropriate data constructor 'all at once' at the end.
        withPreload contName tagName (arityLast, nameLast') (aritiesRest, namesRest) (headName : preloadNamesBackwards) tailName (n - 1)
    mkHandler :: Name -> Name -> [Name] -> (Name, Int) -> Q Exp
    mkHandler lastTail contName preloadedNamesBackwards (conName, arity) = case arity of
      -- All of our arguments have been preloaded already.
      0 -> do
        let headsNames = reverse preloadedNamesBackwards
        conAppE <- foldrM (\headName acc -> AppE acc <$> [e|$punsafeCoerceE $(pure (VarE headName))|]) (ConE conName) headsNames
        pure . AppE (VarE contName) $ conAppE
      -- Some items still remain to be loaded.
      n -> go contName conName preloadedNamesBackwards lastTail (n - 1)
    go :: Name -> Name -> [Name] -> Name -> Int -> Q Exp
    go contName cName headsNamesBackwards lastTailName = \case
      0 -> do
        -- We accumulate the heads needed in reverse order, because otherwise,
        -- this is a quadratic procedure. We can reverse in linear time.
        let headsNames = reverse headsNamesBackwards
        -- Build up applications of all heads to the constructor.
        conAppButLast <- foldrM (\headName acc -> AppE acc <$> [e|$punsafeCoerceE $(pure (VarE headName))|]) (ConE cName) headsNames
        -- Add the last argument by taking the head of the last tail.
        conAppE <- AppE conAppButLast <$> [e|$punsafeCoerceE (pheadList @PData # $(pure (VarE lastTailName)))|]
        -- Hit it with the continuation internally.
        pure . AppE (VarE contName) $ conAppE
      n -> mkUncons (VarE lastTailName) $ \headName tailName ->
        -- We accumulate the needed names of all the heads we have to take
        -- so that we can apply them to the constructor 'all at once' at
        -- the end.
        go contName cName (headName : headsNamesBackwards) tailName (n - 1)

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
          then [e|$(pure . VarE $ selfName) # $(pure . VarE $ fVarName)|]
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
  NonEmptyVector (Name, Vector Type) ->
  Q [Dec]
derivePValidateData tyVars tyName cs =
  [d|
    instance ($ctx) => PValidateData $name where
      pwithValidated d res = pmatch ($punConstrDataE # d) $ \(PBPair tag fields) ->
        $(mkCases 'res 'tag 'fields)
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf [''PValidateData] $ tyVars
    mkCases :: Name -> Name -> Name -> Q Exp
    mkCases contName tagName fieldsName = do
      -- Calculate our preload by finding the minimum number of fields we have
      -- to extract regardless of 'arm'.
      let minArity = Vector.length . snd . NEVector.minimumBy (comparing (Vector.length . snd)) $ cs
      -- Construct the code that will extract minArity fields, leaving behind
      -- an `Exp` referring to the 'remaining tail' if any, as well as all the
      -- 'preloaded' names. Because of silly TH reasons, we have to do this
      -- using CPS.
      withPreload tagName contName minArity (VarE fieldsName) []
    withPreload :: Name -> Name -> Int -> Exp -> [Name] -> Q Exp
    withPreload tagName contName remaining lastTailE fieldNamesBackwards
      | remaining == 0 = do
          let preloadedNames = reverse fieldNamesBackwards
          [e|$punsafeCaseE $(pure . VarE $ tagName) $(mkHandlers contName lastTailE preloadedNames)|]
      | otherwise = mkUncons lastTailE $ \headName tailName -> do
          let remaining' = remaining - 1
          let lastTail' = VarE tailName
          -- This is why they're backwards - accumulating at the end
          -- makes this quadratic, but we can reverse once we're
          -- done in linear time.
          let fieldNamesBackwards' = headName : fieldNamesBackwards
          withPreload tagName contName remaining' lastTail' fieldNamesBackwards'
    mkHandlers :: Name -> Exp -> [Name] -> Q Exp
    mkHandlers contName lastTailE preloadedFieldNames = do
      handlerEs <- traverse (mkHandlerE contName lastTailE preloadedFieldNames . snd) cs
      case NEVector.unsnoc handlerEs of
        (es, e) -> do
          startE <- [e|NEVector.singleton ($toSomeTermE $(pure e))|]
          foldrM (\h acc -> [e|NEVector.cons ($toSomeTermE $(pure h)) $(pure acc)|]) startE es
    mkHandlerE :: Name -> Exp -> [Name] -> Vector Type -> Q Exp
    mkHandlerE contName lastTailE namesLeft fieldTys = case Vector.uncons fieldTys of
      -- We've generated validation code for every field in this 'arm', so
      -- terminate the expression by checking that `lastTailE` is indeed empty.
      Nothing -> terminateList lastTailE contName
      Just (ty, fieldTys') -> do
        let validateE = AppTypeE (VarE 'pwithValidated) ty
        case namesLeft of
          -- We've validated every preloaded name, so we have to do more unconses.
          [] -> mkUncons lastTailE $ \headName tailName -> do
            restE <- mkHandlerE contName (VarE tailName) [] fieldTys'
            [e|$(pure validateE) $(pure . VarE $ headName) ($(pure restE))|]
          -- Some preloaded names remain to validate.
          (n : namesLeft') -> do
            restE <- mkHandlerE contName lastTailE namesLeft' fieldTys'
            [e|$(pure validateE) $(pure . VarE $ n) ($(pure restE))|]
