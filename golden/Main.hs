{-# LANGUAGE BlockArguments #-}

import Data.ByteString.Lazy (ByteString)
import Data.Foldable (traverse_)
import System.Directory (listDirectory, makeAbsolute)
import System.FilePath (replaceExtension, takeBaseName, takeExtension, (<.>), (</>))
import System.IO.Temp (withTempDirectory)
import System.Process.Typed (nullStream, proc, readProcess, runProcess_, setStderr, setStdout, setWorkingDir)
import Test.Hspec (Spec, aroundAll, describe, it, parallel, runIO, shouldBe)
import Test.Hspec.Runner (defaultConfig, evaluateSummary, runSpec)

import qualified Data.ByteString.Lazy as ByteString.Lazy

import Paths_coy (getBinDir)

data TestCase = TestCase
    { testCaseName :: String
    , testCaseCoyExe :: FilePath
    , testCaseInputFile :: FilePath
    , testCaseGoldenFile :: FilePath
    }

getTestCases
    :: FilePath
    -- ^ Path to the `coy` executable.
    -> FilePath
    -- ^ Input directory.
    -> IO [TestCase]
getTestCases coyExe inputDir = fmap (fmap getTestCase . filter isCoySourceFile) (listDirectory inputDir)
  where
    isCoySourceFile = (== ".coy") . takeExtension

    getTestCase inputFileName = TestCase
        { testCaseName = takeBaseName inputFileName
        , testCaseCoyExe = coyExe
        , testCaseInputFile = inputDir </> inputFileName
        , testCaseGoldenFile = inputDir </> replaceExtension inputFileName ".stdout"
        }

runIn
    :: FilePath
    -- ^ Working directory.
    -> FilePath
    -- ^ Executable.
    -> [String]
    -- ^ Arguments.
    -> IO ()
runIn workingDir executable arguments =
    runProcess_
    $ setWorkingDir workingDir
    $ proc executable arguments

runSilentlyIn
    :: FilePath
    -- ^ Working directory.
    -> FilePath
    -- ^ Executable.
    -> [String]
    -- ^ Arguments.
    -> IO ()
runSilentlyIn workingDir executable arguments =
    runProcess_
    $ setStdout nullStream
    $ setStderr nullStream
    $ setWorkingDir workingDir
    $ proc executable arguments

compileAndRunIn
    :: FilePath
    -- ^ Working directory.
    -> FilePath
    -- ^ Path to the @coy@ executable.
    -> FilePath
    -- ^ Input file.
    -> IO ByteString
compileAndRunIn workingDir coy inputFile = do
    -- Run @coy@.
    runIn workingDir coy [inputFile]

    -- Run @clang@.
    runSilentlyIn workingDir "clang" ["-O3", "-o", result1, result0]

    -- Run the resulting executable.
    (_, actual, _) <- readProcess (proc result1 mempty)

    pure actual
  where
    -- Path to the result of @coy@.
    result0 = workingDir </> takeBaseName inputFile <.> ".ll"

    -- Path to the result of @clang@.
    result1 = workingDir </> takeBaseName inputFile

spec :: Spec
spec = do
    coyExe <- runIO (fmap (</> "coy-exe") getBinDir)

    inputDir <- runIO (makeAbsolute "./golden/data")

    testCases <- runIO (getTestCases coyExe inputDir)

    aroundAll (withTempDirectory inputDir mempty) $
        describe "golden" (parallel (traverse_ runTestCase testCases))
  where
    runTestCase testCase =
        it (testCaseName testCase) \workingDir -> do
            actual <- compileAndRunIn workingDir (testCaseCoyExe testCase) (testCaseInputFile testCase)

            expected <- ByteString.Lazy.readFile (testCaseGoldenFile testCase)

            actual `shouldBe` expected

main :: IO ()
main = do
    summary <- runSpec spec defaultConfig
    evaluateSummary summary
