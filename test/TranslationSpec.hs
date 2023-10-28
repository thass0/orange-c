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
    -- NOTE: The exit codes are returned as unsigned 8 bit numbers.
    -- Thus, -6 becomes 250, or ~4 becomes 251.
    it "negated main function" $ do
      exitCode <- compileAndRun negatedMainFunctionSample
      exitCode `shouldBe` 250
    it "logically negated main function (false)" $ do
      exitCode <- compileAndRun logicalNegatedMainFunctionSampleFalse
      exitCode `shouldBe` 0
    it "logically negated main function (true)" $ do
      exitCode <- compileAndRun logicalNegatedMainFunctionSampleTrue
      exitCode `shouldBe` 1
    it "bit-wise negated main function" $ do
      exitCode <- compileAndRun bitwiseNegatedMainFunctionSample
      exitCode `shouldBe` 251
    it "combined unary operators" $ do
      exitCode <- compileAndRun unaryOperatorsMainFunctionSample
      exitCode `shouldBe` 2

-- * Samples

-- NOTE: Sometimes the samples are formatted weirdly. The
-- only purpose of this formatting is to test the parser.

simpleMainFunctionSample :: String
simpleMainFunctionSample = [r|
int main(void) {
  return 2;
}
|]

negatedMainFunctionSample :: String
negatedMainFunctionSample = [r|
int main(void){return -6;}
|]

logicalNegatedMainFunctionSampleFalse :: String
logicalNegatedMainFunctionSampleFalse = [r|
  int

main   (   void
  )
     {
return     !
  6

;

}
|]
  
logicalNegatedMainFunctionSampleTrue :: String
logicalNegatedMainFunctionSampleTrue = [r|
int main(void) {
  return !0;
}
|]

bitwiseNegatedMainFunctionSample :: String
bitwiseNegatedMainFunctionSample = [r|
int main(void) {
  return ~4;
}
|]

unaryOperatorsMainFunctionSample :: String
unaryOperatorsMainFunctionSample = [r|
int main () {
  return
  -   // -> 2
  ~   // -> -2, or 0b111[...]111110
  !   // -> 1
  0;  // -> 0
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
