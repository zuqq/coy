{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import System.FilePath (takeBaseName)

import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy.IO as Text.Lazy.IO
import qualified LLVM.Pretty

import Coy.Codegen
import Coy.Parse
import Coy.Semantic

main :: IO ()
main = do
    [filePath] <- getArgs

    s <- Text.IO.readFile filePath

    case parse s of
        Left e -> do
            putStrLn "A parse error occurred:\n"

            print e
        Right unchecked -> do
            case semantic unchecked of
                Left e -> do
                    putStrLn "A semantic error occurred:\n"

                    print e
                Right checked -> do
                    let n = takeBaseName filePath

                    let m = buildModule n (codegen checked)

                    Text.Lazy.IO.putStrLn (LLVM.Pretty.ppllvm m)
