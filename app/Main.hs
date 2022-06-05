{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (optional)
import Control.Exception (IOException, try)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Exit (exitFailure)
import System.FilePath (takeBaseName, (<.>))
import System.IO (hPutStr, stderr)

import qualified Data.ByteString as ByteString.IO (getContents, putStr, readFile, writeFile)
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy as Text.Lazy
import qualified LLVM.Pretty
import qualified Options.Applicative

import Coy.Codegen
import Coy.Parse
import Coy.Semantic
import Coy.Syntax

data Options = Options
    { moduleNameOption :: Maybe String
    , outputFilePathOption :: Maybe FilePath
    , inputFilePathOption :: FilePath
    }

parseOptionsWithInfo :: Options.Applicative.ParserInfo Options
parseOptionsWithInfo =
    Options.Applicative.info (Options.Applicative.helper <*> parseOptions) $
        Options.Applicative.fullDesc
        <> Options.Applicative.header "Compile `.coy` files to LLVM IR."
  where
    parseOptions =
        Options
        <$> parseModuleNameOption
        <*> parseOutputFilePathOption
        <*> parseInputFilePathOption

    parseModuleNameOption =
        optional . Options.Applicative.strOption $
            Options.Applicative.help "The `ModuleID` of the resulting LLVM IR module."
            <> Options.Applicative.long "module"
            <> Options.Applicative.metavar "<module>"
            <> Options.Applicative.short 'm'

    parseOutputFilePathOption =
        optional . Options.Applicative.strOption $
            Options.Applicative.help "The output file."
            <> Options.Applicative.long "output"
            <> Options.Applicative.metavar "<output>"
            <> Options.Applicative.short 'o'

    parseInputFilePathOption =
        Options.Applicative.strArgument $
            Options.Applicative.help "The input file."
            <> Options.Applicative.metavar "<input>"

tryReadInput :: FilePath -> IO (Either IOException ByteString)
tryReadInput = \case
    "-" -> try ByteString.IO.getContents
    inputFilePath -> try (ByteString.IO.readFile inputFilePath)

readInput :: FilePath -> IO ByteString
readInput inputFilePath = do
    result <- tryReadInput inputFilePath

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
        Right uncheckedModule -> pure uncheckedModule

checkInput :: FilePath -> Text -> Module 'Unchecked -> IO (Module 'Checked)
checkInput inputFilePath input uncheckedModule =
    case semantic inputFilePath input uncheckedModule of
        Left e -> do
            hPutStr stderr e

            exitFailure
        Right checkedModule -> pure checkedModule

tryWriteOutput :: FilePath -> ByteString -> IO (Either IOException ())
tryWriteOutput = \case
    "-" -> try . ByteString.IO.putStr
    outputFilePath -> try . ByteString.IO.writeFile outputFilePath

writeOutput :: FilePath -> ByteString -> IO ()
writeOutput outputFilePath output = do
    result <- tryWriteOutput outputFilePath output

    case result of
        Left e -> do
            hPutStr stderr ("Failed to write output file " <> outputFilePath <> ":\n\n" <> show e)

            exitFailure
        Right () -> mempty

main :: IO ()
main = do
    options <- Options.Applicative.execParser parseOptionsWithInfo

    let inputFilePath = inputFilePathOption options

    let defaultModuleName =
            case inputFilePath of
                "-" -> "from_stdin"
                _ -> takeBaseName inputFilePath

    let moduleName = fromMaybe defaultModuleName (moduleNameOption options)

    let defaultOutputFilePath =
            case inputFilePath of
                "-" -> "-"
                _ -> moduleName <.> ".ll"

    let outputFilePath = fromMaybe defaultOutputFilePath (outputFilePathOption options)

    rawInput <- readInput inputFilePath

    input <- decodeRawInput inputFilePath rawInput

    uncheckedModule <- parseInput inputFilePath input

    checkedModule <- checkInput inputFilePath input uncheckedModule

    let code = codegen moduleName checkedModule

    let bytes = Text.Encoding.encodeUtf8 (Text.Lazy.toStrict (LLVM.Pretty.ppllvm code))

    writeOutput outputFilePath bytes
