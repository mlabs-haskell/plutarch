-- | @since 2.0.0
module Plutarch.Test.Golden
  ( Evaluation (..),
    plutarchGoldenWith,
    debugGolden,
    releaseGolden,
  )
where

import Control.Exception (Exception, throwIO)
import Data.ByteString.Lazy qualified as Lazy
import Data.Char (isSpace, isUpperCase, toLower)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Plutarch.Backend.ANF (analyzeDeps, fromHashedAST, fullPipeline)
import Plutarch.Backend.AST (fromRawTerm)
import Plutarch.Backend.Compile (toUPLCTerm)
import Plutarch.Backend.Term
  ( OptimizationMode (InternalExternal, OnlyInternal),
    S,
    Term,
    TermEnv (TermEnv),
    TermError,
    debugTermEnv,
    releaseTermEnv,
  )
import Plutarch.Backend.UPLC (UPLCTerm, uplcConstant)
import Plutarch.Helpers.Compile (compileTerm)
import Plutarch.Helpers.Evaluate (evalUPLC, maxBudget)
import PlutusCore qualified as PLC
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget)
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutSmart)
import Prettyprinter.Render.Text (renderLazy)
import System.FilePath ((<.>), (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as Cek

-- | @since 2.0.0
data Evaluation
  = EvaluateUPLC
  | Don'tEvaluateUPLC
  deriving stock
    ( -- | @since 2.0.0
      Eq,
      -- | @since 2.0.0
      Show
    )

-- | Constructs golden files based on the given closed 'Term'. Will always
-- generate golden files of each of the following:
--
-- * The 'Term' itself;
-- * Its 'AST';
-- * Its 'ANF', both before, and after, the processing pipeline; and
-- * The resulting UPLC.
--
-- If given 'EvaluateUPLC' as an argument, it will also evaluate the resulting
-- UPLC, and produce a golden file of its result. A 'TermEnv' for compilation is
-- provided to determine how much tracing, or what optimizations, to use.
--
-- All but the first of these will produce an error if the 'Term' fails to
-- compile.
--
-- = Important note
--
-- Ensure that the test name is unique to all tests in your suite. This will be
-- used to determine the name of the folder where the golden files will be
-- placed : if there is a name collision, this will produce strange failures.
--
-- @since 2.0.0
plutarchGoldenWith ::
  forall (a :: S -> Type).
  -- | Should we evaluate the result?
  Evaluation ->
  -- | Optimization and tracing level.
  TermEnv ->
  -- | A description for the test. This is what you will see when you run the
  -- test. Does not have to be unique.
  String ->
  -- | A name for the test. This will determine the golden file names and
  -- locations, and thus, should be unique, as described above.
  String ->
  -- | A closed 'Term'.
  (forall (s :: S). Term s a) ->
  TestTree
plutarchGoldenWith evaluation env@(TermEnv _ opt) testDescription testName t =
  let folderName = toFolderName testName
      goldenFolderFP = "golden" </> folderName
      termGoldenFP = goldenFolderFP </> "term" <.> "golden"
      compiled = compileTerm env t
      asAST = fromRawTerm <$> compiled
      astGoldenFP = goldenFolderFP </> "ast" <.> "golden"
      asANF = fromHashedAST <$> asAST
      anfGoldenFP = goldenFolderFP </> "anf" <.> "golden"
      withDeps = analyzeDeps <$> asANF
      anfDepsFP = goldenFolderFP </> "anf-deps" <.> "golden"
      withDemand = fullPipeline <$> withDeps
      demandGoldenFP = goldenFolderFP </> "anf-demand" <.> "golden"
      optAsBool = case opt of OnlyInternal -> False; InternalExternal -> True
      asUPLC = toUPLCTerm optAsBool <$> withDemand
      uplcGoldenFP = goldenFolderFP </> "uplc" <.> "golden"
      mandatoryTests =
        [ goldenVsString "Term" termGoldenFP (pure . toLazyBS $ t),
          goldenVsString "AST" astGoldenFP (toLazyBSOrErr asAST),
          goldenVsString "ANF" anfGoldenFP (toLazyBSOrErr asANF),
          goldenVsString "ANF with dependency analysis" anfDepsFP (toLazyBSOrErr withDeps),
          goldenVsString "ANF with demand analysis" demandGoldenFP (toLazyBSOrErr withDemand),
          goldenVsString "UPLC" uplcGoldenFP (toLazyBSOrErr asUPLC)
        ]
   in testGroup (testName <> ": " <> testDescription) $ case evaluation of
        Don'tEvaluateUPLC -> mandatoryTests
        EvaluateUPLC ->
          let evaluated = evalUPLC maxBudget <$> asUPLC
              uplcEvalGoldenFP = goldenFolderFP </> "uplc-eval" <.> "golden"
           in mandatoryTests
                <> [goldenVsString "UPLC (evaluated)" uplcEvalGoldenFP (toLazyBSEvaluated evaluated)]

-- | As 'plutachGoldenWith', with the 'TermEnv' set to 'debugTermEnv'.
--
-- @since 2.0.0
debugGolden ::
  forall (a :: S -> Type).
  -- | Should we evaluate the result?
  Evaluation ->
  -- | A description for the test. This is what you will see when you run the
  -- test. Does not have to be unique.
  String ->
  -- | A name for the test. This will determine the golden file names and
  -- locations, and thus, should be unique, as described above.
  String ->
  -- | A closed 'Term'.
  (forall (s :: S). Term s a) ->
  TestTree
debugGolden evaluation = plutarchGoldenWith evaluation debugTermEnv

-- | As 'plutarchGoldenWith', with the 'TermEnv' set to 'releaseTermEnv'.
--
-- @since 2.0.0
releaseGolden ::
  forall (a :: S -> Type).
  -- | Should we evaluate the result?
  Evaluation ->
  -- | A description for the test. This is what you will see when you run the
  -- test. Does not have to be unique.
  String ->
  -- | A name for the test. This will determine the golden file names and
  -- locations, and thus, should be unique, as described above.
  String ->
  -- | A closed 'Term'.
  (forall (s :: S). Term s a) ->
  TestTree
releaseGolden evaluation = plutarchGoldenWith evaluation releaseTermEnv

-- Helpers

-- Replace whitespace with dash, downcase everything
toFolderName :: String -> FilePath
toFolderName s = go <$> s
  where
    go :: Char -> Char
    go c
      | isSpace c = '-'
      | isUpperCase c = toLower c
      | otherwise = c

toLazyBS :: forall (a :: Type). (Pretty a) => a -> Lazy.ByteString
toLazyBS = encodeUtf8 . renderLazy . layoutSmart defaultLayoutOptions . pretty

toLazyBSOrErr ::
  forall (a :: Type).
  (Pretty a) =>
  Either TermError a ->
  IO Lazy.ByteString
toLazyBSOrErr = \case
  Left err -> throwIO . DidNotCompileException $ err
  Right res -> pure . toLazyBS $ res

toLazyBSEvaluated ::
  Either
    TermError
    ( Either
        (Cek.CekEvaluationException PLC.Name PLC.DefaultUni PLC.DefaultFun)
        (Either (PLC.Some (PLC.ValueOf PLC.DefaultUni)) UPLCTerm),
      ExBudget,
      [Text]
    ) ->
  IO Lazy.ByteString
toLazyBSEvaluated = \case
  Left err -> throwIO . DidNotCompileException $ err
  Right (Left err, _, _) -> throwIO . DidNotEvaluateException $ err
  Right (Right (Left c), _, _) -> pure . toLazyBS . uplcConstant $ c
  Right (Right (Right t), _, _) -> pure . toLazyBS $ t

newtype DidNotCompileException = DidNotCompileException TermError
  deriving stock (Show)

instance Exception DidNotCompileException

newtype DidNotEvaluateException
  = DidNotEvaluateException (Cek.CekEvaluationException PLC.Name PLC.DefaultUni PLC.DefaultFun)
  deriving stock (Show)

instance Exception DidNotEvaluateException
