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

spec :: Spec
spec = do
    let examplesDirectory = "examples"

    examplesFileNames <- runIO (getCoySourceFileNames examplesDirectory)

    describe examplesDirectory (
        for_ examplesFileNames (\fileName ->
            it fileName (shouldParse (examplesDirectory </> fileName))))

    let goldenDirectory = "test/golden"

    goldenFileNames <- runIO (getCoySourceFileNames goldenDirectory)

    describe goldenDirectory (
        for_ goldenFileNames (\fileName ->
            it fileName (shouldParse (goldenDirectory </> fileName))))

    let badDirectory = "test/parse/bad"

    badFileNames <- runIO (getCoySourceFileNames badDirectory)

    describe badDirectory (
        for_ badFileNames (\fileName ->
            it fileName (shouldNotParse (badDirectory </> fileName))))
  where
    isCoySourceFile fileName = takeExtension fileName == ".coy"

    getCoySourceFileNames folder =
        fmap (filter isCoySourceFile) (listDirectory folder)
