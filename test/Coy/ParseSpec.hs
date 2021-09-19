module Coy.ParseSpec (spec) where

import Data.Foldable (for_)
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import Test.Hspec

import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Text.Encoding

import Coy.Parse

shouldParse :: FilePath -> Expectation
shouldParse filePath = do
    rawInput <- ByteString.readFile filePath
    case Text.Encoding.decodeUtf8' rawInput of
        Left e -> expectationFailure (show e)
        Right s ->
            case parse filePath s of
                Left e -> expectationFailure (show e)
                Right _ -> mempty

shouldNotParse :: FilePath -> Expectation
shouldNotParse filePath = do
    rawInput <- ByteString.readFile filePath
    case Text.Encoding.decodeUtf8' rawInput of
        Left e -> expectationFailure (show e)
        Right s ->
            case parse filePath s of
                Left _ -> mempty
                Right _ -> expectationFailure "Unexpected successful parse."

isCoySourceFile :: FilePath -> Bool
isCoySourceFile fileName = takeExtension fileName == ".coy"

getCoySourceFileNames :: FilePath -> IO [FilePath]
getCoySourceFileNames directory =
    fmap (filter isCoySourceFile) (listDirectory directory)

forDirectory :: FilePath -> (FilePath -> Expectation) -> Spec
forDirectory directory f = do
    fileNames <- runIO (getCoySourceFileNames directory)
    describe directory (
        for_ fileNames (\fileName -> it fileName (f (directory </> fileName))))

spec :: Spec
spec = do
    forDirectory "examples" shouldParse

    forDirectory "test/golden" shouldParse

    forDirectory "test/parse/bad" shouldNotParse
