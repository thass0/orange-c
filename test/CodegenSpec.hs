module CodegenSpec (spec) where

import Test.Hspec
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty
import qualified Data.Text as T

import Codegen
import Ast

spec :: Spec
spec = do
  describe "Code generation" $ do
    expressions
    programs


expressions :: Spec
expressions = do
  describe "expressions" $ do
    it "arithmetic negation" $
      asm (Negate (Constant 9))
      `shouldBe`
      fInstrs
      [ "pushq $9"
      , "popq %rax"
      , "neg %eax"
      , "pushq %rax"
      ]

    it "logical negation" $
      asm (LogicNot (Constant 94))
      `shouldBe`
      fInstrs
      [ "pushq $94"
      , "popq %rax"
      , "cmpl $0, %eax"
      , "sete %al"
      , "movzbl %al, %eax"
      , "pushq %rax"
      ]

    it "bit-wise negation" $
      asm (BitNot (Constant 42))
      `shouldBe`
      fInstrs
      [ "pushq $42"  -- Constant 42
      , "popq %rax"  -- Start of not
      , "notl %eax"
      , "pushq %rax"  -- End of not
      ]

    it "addition" $
      asm (Add (Constant 4) (Constant 5))
      `shouldBe`
      fInstrs
      [ "pushq $4"  -- Constant 4
      , "pushq $5"  -- Constant 5
      , "popq %rdx"  -- Start of addition
      , "popq %rax"
      , "addl %edx, %eax"  -- eax <- eax +  edx
      , "pushq %rax"  -- End of addition
      ]
    it "division" $
      asm (Div (Constant 18) (Constant 9))
      `shouldBe`
      fInstrs
      [ "pushq $18"
      , "pushq $9"
      , "popq %rbx"
      , "popq %rax"
      , "cltd"
      , "idivl %ebx"
      , "pushq %rax"
      ]

    it "logical AND and logical OR" $
      -- Both 5 and 2 are non-zero and thus considered true.
      asm (LogicOr (LogicAnd (Constant 5) (Constant 2)) (Constant 3))
      `shouldBe`
      (T.unlines $ fInstrs'
      -- Start of logical AND
      [ "pushq $5"  -- Constant 5 (lhs of AND)
      , "pushq $2"  -- Constant 2 (rhs of AND)
      , "popq %rdx"
      , "popq %rax"
      , "cmpl $0, %eax"
      , "je .L0"
      , "cmpl $0, %edx"
      , "je .L0"
      , "movl $1, %eax"
      , "jmp .L1"
      ] <>
      fLabel' ".L0" <>
      fInstr "movl $0, %eax" <>
      fLabel' ".L1" <>
      fInstr "pushq %rax" <>
      -- End of logical AND (lhs of OR)
      -- Start of logical OR
      fInstrs'
      [ "pushq $3"  -- Constant 3 (rhs of OR)
      , "popq %rdx"
      , "popq %rax"
      , "cmpl $0, %eax"
      , "jne .L2"
      , "cmpl $0, %edx"
      , "jne .L2"
      , "movl $0, %eax"
      , "jmp .L3"
      ] <>
      fLabel' ".L2" <>
      fInstr "movl $1, %eax" <>
      fLabel' ".L3" <>
      fInstr "pushq %rax")
      -- End of logical OR
      
programs :: Spec
programs = do
  describe "programs" $ do
    it "main function" $
      asm (Program
                   (singleton (Function "main"
                               (singleton (Return (Constant 2))))))
      `shouldBe`
      "main:\n\tpushq $2\n\tpopq %rax\n\tret\n"
