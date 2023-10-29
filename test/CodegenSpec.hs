module CodegenSpec (spec) where

import Test.Hspec
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty

import qualified Codegen
import Ast

spec :: Spec
spec = do
  describe "Code generation" $ do
    expressions
    programs


expressions :: Spec
expressions = do
  describe "statements" $ do
    it "arithmetic negation" $
      genAsmL (Negate (Constant 9))
      `shouldBe`
      [ "\tpushq $9"
      , "\tpopq %rax"
      , "\tneg %eax"
      , "\tpushq %rax"
      ]

    it "logical negation" $
      genAsmL (LogicNot (Constant 94))
      `shouldBe`
      [ "\tpushq $94"
      , "\tpopq %rax"
      , "\tcmpl $0, %eax"
      , "\tsete %al"
      , "\tmovzbl %al, %eax"
      , "\tpushq %rax"
      ]

    it "bit-wise negation" $
      genAsmL (BitNot (Constant 42))
      `shouldBe`
      [ "\tpushq $42"  -- Constant 42
      , "\tpopq %rax"  -- Start of not
      , "\tnotl %eax"
      , "\tpushq %rax"  -- End of not
      ]
    it "addition" $
      genAsmL (Add (Constant 4) (Constant 5))
      `shouldBe`
      [ "\tpushq $4"  -- Constant 4
      , "\tpushq $5"  -- Constant 5
      , "\tpopq %rdx"  -- Start of addition
      , "\tpopq %rax"
      , "\taddl %edx, %eax"  -- eax <- eax +  edx
      , "\tpushq %rax"  -- End of addition
      ]
    it "division" $
      genAsmL (Div (Constant 18) (Constant 9))
      `shouldBe`
      [ "\tpushq $18"
      , "\tpushq $9"
      , "\tpopq %rbx"
      , "\tpopq %rax"
      , "\tcltd"
      , "\tidivl %ebx"
      , "\tpushq %rax"
      ]

programs :: Spec
programs = do
  describe "programs" $ do
    it "main function" $
      Codegen.asm (Program
                   (singleton (Function "main"
                               (singleton (Return (Constant 2))))))
      `shouldBe`
      "main:\n\tpushq $2\n\tpopq %rax\n\tret\n"


-- * Helpers

genAsmL :: (Codegen.GenAsm a) => a -> [Codegen.Asm]
genAsmL node = NonEmpty.toList $ Codegen.genAsm node
