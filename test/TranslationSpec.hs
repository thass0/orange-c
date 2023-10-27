{-# language QuasiQuotes #-}

module TranslationSpec (spec) where

import Data.ByteString as BS
import Data.ByteString.Char8 as B8
import System.Process.Typed
import System.Directory
import Test.Hspec
import Text.RawString.QQ

import qualified OrangeC

spec :: Spec
spec = do
  describe "Language translation tests" $ do
    simple


simple :: Spec
simple = do
  describe "Simple translation tests" $ do
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
      asm = OrangeC.compile $ B8.pack src
  writeAsmToFile asmFile asm
  assemble asmFile execFile
  exitCode <- runProcess $ shell ("./" <> execFile)
  removeFile asmFile
  removeFile execFile
  pure $ case exitCode of
    ExitSuccess -> 0
    ExitFailure e -> e

  where
    writeAsmToFile :: FilePath -> ByteString -> IO ()
    writeAsmToFile = BS.writeFile

    assemble :: FilePath -> FilePath -> IO ()
    assemble asmFile execFile =
      runProcess_ $ setStdin closed $ proc "gcc" [asmFile, "-o", execFile]

