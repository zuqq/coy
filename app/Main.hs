{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (liftA2, optional)
import Control.Exception (IOException, throwIO, try)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Exit (exitFailure)
import System.FilePath (takeBaseName, (<.>))
import System.IO (hPutStr, stderr)

import qualified Data.ByteString as ByteString.IO (readFile, writeFile)
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy as Text.Lazy
import qualified LLVM.Pretty
import qualified Options.Applicative

import Coy.Codegen
import Coy.Parse
import Coy.Semantic
import Coy.Syntax

data Options = Options
    { inputFilePathOption :: FilePath
    , outputFilePathOption :: Maybe FilePath
    }

parseOptionsWithInfo :: Options.Applicative.ParserInfo Options
parseOptionsWithInfo =
    Options.Applicative.info (Options.Applicative.helper <*> parseOptions) $
        Options.Applicative.fullDesc
        <> Options.Applicative.header "Compile `.coy` files to LLVM IR."
  where
    parseOptions = liftA2 Options parseInputFilePathOption parseOutputFilePathOption

    parseInputFilePathOption =
        Options.Applicative.strArgument $
            Options.Applicative.help "The input file."
            <> Options.Applicative.metavar "<input>"

    parseOutputFilePathOption =
        optional . Options.Applicative.strOption $
            Options.Applicative.help "The output file."
            <> Options.Applicative.long "output"
            <> Options.Applicative.metavar "<output>"
            <> Options.Applicative.short 'o'

tryReadFile :: FilePath -> IO (Either IOException ByteString)
tryReadFile = try . ByteString.IO.readFile

readInputFile :: FilePath -> IO ByteString
readInputFile inputFilePath = do
    result <- tryReadFile inputFilePath

    case result of
        Left e -> do
            hPutStr stderr ("Failed to read input file " <> inputFilePath <> ":\n\n" <> show e)

            exitFailure
        Right rawInput -> pure rawInput

decodeRawInput :: FilePath -> ByteString -> IO Text
decodeRawInput inputFilePath rawInput =
    case Text.Encoding.decodeUtf8' rawInput of
        Left e -> do
            hPutStr stderr ("Failed to decode input file " <> inputFilePath <> " as UTF-8:\n\n" <> show e)

            exitFailure
        Right input -> pure input

parseInput :: FilePath -> Text -> IO (Module 'Unchecked)
parseInput inputFilePath input =
    case parse inputFilePath input of
        Left e -> do
            hPutStr stderr e

            exitFailure
        Right unchecked -> pure unchecked

checkInput :: FilePath -> Text -> Module 'Unchecked -> IO (Module 'Checked)
checkInput inputFilePath input unchecked =
    case semantic inputFilePath input unchecked of
        Left e -> do
            hPutStr stderr e

            exitFailure
        Right checked -> pure checked

tryWriteFile :: FilePath -> ByteString -> IO (Either IOException ())
tryWriteFile outputFilePath = try . ByteString.IO.writeFile outputFilePath

writeOutput :: FilePath -> ByteString -> IO ()
writeOutput outputFilePath output = do
    result <- tryWriteFile outputFilePath output

    case result of
        Left e -> do
            hPutStr stderr ("Failed to write output file " <> outputFilePath <> ":\n\n" <> show e)

            exitFailure
        Right () -> mempty

main :: IO ()
main = do
    options <- Options.Applicative.execParser parseOptionsWithInfo

    let inputFilePath = inputFilePathOption options

    let outputFilePath = fromMaybe (takeBaseName inputFilePath <.> "ll") (outputFilePathOption options)

    rawInput <- readInputFile inputFilePath

    input <- decodeRawInput inputFilePath rawInput

    uncheckedModule <- parseInput inputFilePath input

    checkedModule <- checkInput inputFilePath input uncheckedModule

    let code = codegen (takeBaseName inputFilePath) checkedModule

    let bytes = Text.Encoding.encodeUtf8 (Text.Lazy.toStrict (LLVM.Pretty.ppllvm code))

    writeOutput outputFilePath bytes
