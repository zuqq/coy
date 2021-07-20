{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((<.>), takeBaseName)
import System.IO (hPutStr, stderr)

import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy.IO as Text.Lazy.IO
import qualified LLVM.Pretty
import qualified Text.Pretty.Simple

import Coy.Codegen
import Coy.Parse
import Coy.Semantic

main :: IO ()
main = do
    [filePath] <- getArgs

    s <- Text.IO.readFile filePath

    case parse s of
        Left e -> do
            hPutStr stderr "A parse error occurred:\n\n"

            Text.Pretty.Simple.pHPrintNoColor stderr e

            exitFailure
        Right unchecked -> do
            case semantic unchecked of
                Left e -> do
                    hPutStr stderr "A semantic error occurred:\n\n"

                    Text.Pretty.Simple.pHPrintNoColor stderr e

                    exitFailure
                Right checked -> do
                    let n = takeBaseName filePath

                    let m = buildModule n (codegen checked)

                    Text.Lazy.IO.writeFile (n <.> "ll") (LLVM.Pretty.ppllvm m)
