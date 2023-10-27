module TranslationSpec (spec) where

import System.Process.Typed
import System.Directory
import Test.Hspec
import Text.RawString.QQ
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO

import qualified OrangeC

spec :: Spec
spec = do
  describe "Language translation" $ do
    simple


simple :: Spec
simple = do
  describe "Simple translation" $ do
    it "main function" $ do
      exitCode <- compileAndRun simpleMainFunctionSample
      exitCode `shouldBe` 2


-- * Samples

simpleMainFunctionSample :: String
simpleMainFunctionSample = [r|
int main(void) {
  return 2;
}
|]


-- * Helpers

-- | Compile the given source code and run it to returns its exit code.
compileAndRun :: String -> IO Int
compileAndRun src = do
  let asmFile = "test/test.s"
      execFile = "test/test.out"
      asm' = OrangeC.compile "testFile.c" (T.pack src)
  case asm' of
    Left err -> error ("Error: " ++ err)
    Right asm -> do
      TextIO.writeFile asmFile asm
      assemble asmFile execFile
      exitCode <- runProcess $ shell ("./" <> execFile)
      removeFile asmFile
      removeFile execFile
      pure $ case exitCode of
               ExitSuccess -> 0
               ExitFailure e -> e

  where
    assemble :: FilePath -> FilePath -> IO ()
    assemble asmFile execFile =
      runProcess_ $ setStdin closed $ proc "gcc" [asmFile, "-o", execFile]
