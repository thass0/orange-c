module Codegen
  (asm
  , prelude
  , Asm
  , GenAsm (..)
  ) where

import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty ((<|))
import qualified Data.Text as T

import Ast

-- | A piece of assembly code.
type Asm = T.Text

-- | Generate an assembler prelude that starts the program
--   in the given function.
prelude :: Ident -> Asm -> Asm
prelude entrypoint code = T.unlines
  [ "\t.text"
  , "\t.global " <> entrypoint
  , "\t.type   " <> entrypoint <> ", @function"
  , "\t.p2align 4"
  , ""
  , code
  , ""
  , "\t.section .note.GNU-stack,\"\",@progbits"
  ]

-- | Generate assembly code from the given AST.
asm :: Ast -> Asm
asm = T.unlines . NonEmpty.toList . genAsm

-- | Something that can generate assembly code for itself.
class GenAsm a where
  genAsm :: a -> NonEmpty.NonEmpty Asm

-- * Assembly generation for AST nodes.

instance GenAsm Program where
  genAsm (Program functions) = functions >>= genAsm

instance GenAsm Function where
  genAsm (Function ident statements) =
    (ident <> ":") <| (statements >>= genAsm)

instance GenAsm Statement where
  genAsm (Return expr) =
    genAsm expr <> instrs ["popq %rax", "ret"]

instance GenAsm Expression where
  genAsm thisExpr = case thisExpr of
    (Constant i) -> instr $ "pushq " <> asmLit i
    (Negate expr) -> genAsm expr <> unaryInstr "neg %eax"
    (LogicNot expr) -> genAsm expr <> unaryInstrs
      [ "cmpl $0, %eax"
      , "sete %al"
      , "movzbl %al, %eax"
      ]
    (BitNot expr) -> genAsm expr <> unaryInstr "notl %eax"
    (Add lhs rhs) -> genAsm lhs <>
                     genAsm rhs <>
                     binaryInstr "addl %edx, %eax"
    (Sub lhs rhs) -> genAsm lhs <>
                     genAsm rhs <>
                     binaryInstr "subl %edx, %eax"
    (Mul lhs rhs) -> genAsm lhs <>
                     genAsm rhs <>
                     binaryInstr "imull %edx, %eax"
    (Div lhs rhs) -> genAsm lhs <>
                     genAsm rhs <>
                     instrs
                     [ "popq %rbx"   -- rhs, divisor
                     , "popq %rax"   -- lhs, dividend. It's sign extended into
                     , "cltd"        -- edx, since `idivq` divides `edx:eax`
                     , "idivl %ebx"  -- by the given divisor (here `ebx`).
                     , "pushq %rax"  -- Quotient is found in `eax`.
                     ]


-- * Helpers

-- | A unary instruction that operates on `%eax`.
unaryInstr :: Asm -> NonEmpty.NonEmpty Asm
unaryInstr i = instrs
  [ "popq %rax"
  , i
  , "pushq %rax"
  ]

-- | A list of unary instructions that operate on `%eax`.
unaryInstrs :: [Asm] -> NonEmpty.NonEmpty Asm
unaryInstrs is = instr "popq %rax" <> instrs is <> instr "pushq %rax"

-- | A binary instruction that expects its left hand side
--   argument in `%eax` and its right hand side argument
--   in `%edx`.
binaryInstr :: Asm -> NonEmpty.NonEmpty Asm
binaryInstr i = instr "popq %rdx" <>
                instr "popq %rax" <>
                instr i <>
                instr "pushq %rax"

-- | Wrap a single instruction in a non-empty singleton.
--   Indents the instruction with a single tab.
instr :: Asm -> NonEmpty.NonEmpty Asm
instr = NonEmpty.singleton . T.cons '\t'

-- | Wrap a list of instructions in a non-empty list.
--   Indents the instructions with a single tab each.
instrs :: [Asm] -> NonEmpty.NonEmpty Asm
instrs = NonEmpty.fromList . map (T.cons '\t')

-- | Show as a GAS assembly literal.
asmLit :: Int -> Asm
asmLit = T.pack . ('$' :) . show
