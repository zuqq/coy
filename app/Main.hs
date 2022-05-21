{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (liftA2, optional)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((<.>), takeBaseName)
import System.IO (IOMode (WriteMode), hPutStr, stderr, withFile)

import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy.IO as Text.Lazy.IO
import qualified LLVM.Pretty
import qualified Options.Applicative

import Coy.Codegen
import Coy.Parse
import Coy.Semantic

data Options = Options
    { input :: FilePath
    , output :: Maybe FilePath
    }

parseOptionsWithInfo :: Options.Applicative.ParserInfo Options
parseOptionsWithInfo =
    Options.Applicative.info (Options.Applicative.helper <*> parseOptions) $
        Options.Applicative.fullDesc
        <> Options.Applicative.header "Compile `.coy` files to LLVM IR."
  where
    parseOptions = liftA2 Options parseInput (optional parseOutput)

    parseInput =
        Options.Applicative.strArgument $
            Options.Applicative.help "The input file."
            <> Options.Applicative.metavar "<input>"

    parseOutput =
        Options.Applicative.strOption $
            Options.Applicative.long "output"
            <> Options.Applicative.short 'o'
            <> Options.Applicative.help "The output file."
            <> Options.Applicative.metavar "<output>"

main :: IO ()
main = do
    options <- Options.Applicative.execParser parseOptionsWithInfo

    let inputFilePath = input options

    let outputFilePath = fromMaybe (takeBaseName inputFilePath <.> "ll") (output options)

    s <- Text.IO.readFile inputFilePath

    case parse inputFilePath s of
        Left e -> do
            hPutStr stderr e

            exitFailure
        Right unchecked -> do
            case semantic inputFilePath s unchecked of
                Left e -> do
                    hPutStr stderr e

                    exitFailure
                Right checked -> do
                    let m = codegen inputFilePath checked

                    withFile outputFilePath WriteMode \handle ->
                        Text.Lazy.IO.hPutStrLn handle (LLVM.Pretty.ppllvm m)
