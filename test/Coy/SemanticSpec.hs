{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}

module Coy.SemanticSpec (spec) where

import Data.Foldable (traverse_)
import System.Directory (listDirectory)
import System.FilePath ((</>), takeBaseName, takeExtension)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, runIO)

import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Text.Encoding

import Coy.Parse
import Coy.Semantic
import Coy.Syntax

data TestCase = TestCase
    { testCaseName :: String
    , testCaseInputFile :: FilePath
    }

getTestCases :: FilePath -> IO [TestCase]
getTestCases inputDir = fmap (fmap getTestCase . filter isCoySourceFile) (listDirectory inputDir)
  where
    isCoySourceFile = (== ".coy") . takeExtension

    getTestCase inputFileName = TestCase
        { testCaseName = takeBaseName inputFileName
        , testCaseInputFile = inputDir </> inputFileName
        }

parseAndAnalyze :: FilePath -> IO (Either String (Module 'Checked))
parseAndAnalyze filePath = do
    rawInput <- ByteString.readFile filePath
    case Text.Encoding.decodeUtf8' rawInput of
        Left e -> error ("Failed to decode:\n\n" <> show e)
        Right s ->
            case parse filePath s of
                Left e -> error ("Failed to parse:\n\n" <> e)
                Right m -> pure (semantic filePath s m)

shouldPass :: FilePath -> Expectation
shouldPass filePath = do
    result <- parseAndAnalyze filePath
    case result of
        Left e -> expectationFailure ("Expected success, got:\n\n" <> e)
        Right _ -> mempty

shouldNotPass :: FilePath -> Expectation
shouldNotPass filePath = do
    result <- parseAndAnalyze filePath
    case result of
        Left _ -> mempty
        Right m -> expectationFailure ("Expected failure, got:\n\n" <> show m)

forDirectory :: FilePath -> (FilePath -> Expectation) -> Spec
forDirectory directory check = do
    testCases <- runIO (getTestCases directory)

    describe directory (traverse_ runTestCase testCases)
  where
    runTestCase testCase = it (testCaseName testCase) (check (testCaseInputFile testCase))

spec :: Spec
spec = do
    forDirectory "examples" shouldPass

    forDirectory "golden/data" shouldPass

    forDirectory "test/data/semantic/bad" shouldNotPass
