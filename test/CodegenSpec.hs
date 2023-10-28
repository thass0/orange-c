module CodegenSpec (spec) where

import Test.Hspec
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty

import qualified Codegen
import Ast

spec :: Spec
spec = do
  describe "Code generation" $ do
    it "main function ast" $
      Codegen.asm (Program
                   (singleton (Function "main"
                               (singleton (Return (Constant 2))))))
      `shouldBe`
      "main:\n\tmovl $2, %eax\n\tret\n"

    it "negated return ast" $
      genAsmList (Return (Negate (Constant 9)))
      `shouldBe`
      [ "\tmovl $9, %eax"
      , "\tneg %eax"
      , "\tret"
      ]

    it "logically negated return ast" $
      genAsmList (Return (LogicNot (Constant 94)))
      `shouldBe`
      [ "\tmovl $94, %eax"
      , "\tcmpl $0, %eax"
      , "\tsete %al"
      , "\tmovzbl %al, %eax"
      , "\tret"
      ]

    it "bit-wise complemented return ast" $
      genAsmList (Return (BitNot (Constant 42)))
      `shouldBe`
      [ "\tmovl $42, %eax"
      , "\tnotl %eax"
      , "\tret"
      ]


-- * Helpers

genAsmList :: (Codegen.GenAsm a) => a -> [Codegen.Asm]
genAsmList node = NonEmpty.toList $ Codegen.genAsm node
