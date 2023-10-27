module CodegenSpec (spec) where

import Test.Hspec

import qualified Codegen
import Parse
import Data.List.NonEmpty

spec :: Spec
spec = do
  describe "Code generation" $ do
    it "main function ast" $
      Codegen.asm (Program
                   (singleton (Function "main"
                               (singleton (Return (Constant 2))))))
      `shouldBe`
      "main:\n\tmovl $2, %eax\n\tret\n"
