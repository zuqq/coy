{-# LANGUAGE BlockArguments #-}

module Coy.ParseSpec (spec) where

import Data.Foldable (traverse_)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, runIO)

import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Text.Encoding

import Coy.Parse

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

shouldParse :: FilePath -> Expectation
shouldParse filePath = do
    rawInput <- ByteString.readFile filePath
    case Text.Encoding.decodeUtf8' rawInput of
        Left e -> expectationFailure (show e)
        Right s ->
            case parse filePath s of
                Left e -> expectationFailure ("Expected success, got:\n\n" <> e)
                Right _ -> mempty

shouldNotParse :: FilePath -> Expectation
shouldNotParse filePath = do
    rawInput <- ByteString.readFile filePath
    case Text.Encoding.decodeUtf8' rawInput of
        Left e -> expectationFailure (show e)
        Right s ->
            case parse filePath s of
                Left _ -> mempty
                Right e -> expectationFailure ("Expected failure, got:\n\n" <> show e)

forDirectory :: FilePath -> (FilePath -> Expectation) -> Spec
forDirectory directory check = do
    testCases <- runIO (getTestCases directory)

    describe directory (traverse_ runTestCase testCases)
  where
    runTestCase testCase = it (testCaseName testCase) (check (testCaseInputFile testCase))

spec :: Spec
spec = do
    forDirectory "examples" shouldParse

    forDirectory "golden/data" shouldParse

    forDirectory "test/data/parse/bad" shouldNotParse

    forDirectory "test/data/parse/good" shouldParse
