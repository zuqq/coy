import Data.Foldable (for_)
import System.Directory (listDirectory, makeAbsolute)
import System.FilePath ((<.>), (</>), dropExtension, replaceExtension, takeExtension, takeFileName)
import System.IO.Temp (withTempDirectory)
import System.Process.Typed (nullStream, proc, readProcess, runProcess_, setStderr, setWorkingDir)
import Test.Hspec (describe, it, shouldBe)
import Test.Hspec.Runner (defaultConfig, evaluateSummary, runSpec)

import qualified Data.ByteString.Lazy as ByteString.Lazy

main :: IO ()
main = do
    goldenDirectory <- makeAbsolute "./golden"

    let goldenDataDirectory = goldenDirectory </> "data"

    fileNames <- getCoySourceFileNames goldenDataDirectory

    withTempDirectory goldenDirectory "data" (\tempDirectory ->
        runAndEvaluate (
            describe "golden/data" (
                for_ fileNames (\fileName -> it fileName (do
                    let filePath = goldenDataDirectory </> fileName

                    runProcess_ (setWorkingDir tempDirectory (proc "stack" ["run", "--", filePath]))

                    let executableFilePath = tempDirectory </> dropExtension fileName

                    let resultFilePath = tempDirectory </> dropExtension fileName <.> ".ll"

                    runProcess_ (
                        setStderr nullStream (
                            proc "clang" ["-O3", "-o", executableFilePath, resultFilePath]))

                    (_, actual, _) <- readProcess (proc executableFilePath mempty)

                    expected <- ByteString.Lazy.readFile (replaceExtension filePath ".stdout")

                    actual `shouldBe` expected)))))
  where
    isCoySourceFile fileName = takeExtension fileName == ".coy"

    getCoySourceFileNames directory =
        fmap (filter isCoySourceFile) (listDirectory directory)

    runAndEvaluate spec = do
        summary <- runSpec spec defaultConfig
        evaluateSummary summary
