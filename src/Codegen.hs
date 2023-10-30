module Codegen
  ( asm
  , prelude
  , GenAsm (..)
  , fInstr
  , fInstrs
  , fLabel
  ) where

import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty ((<|))
import qualified Data.Text as T
import qualified Data.Map as Map

import Ast

-- | A intermediate representation of operations expressed
--   in terms of assembler code.
data Asm
  = APush Int
  | ARet
  | ANegate
  | ALogicNot
  | ABitNot
  | AAdd
  | ASub
  | AMul
  | ADiv
  | ALogicAnd
  | ALogicOr
  | ALabel Ident
  deriving (Show, Eq)

-- | Generate assembly code from the given AST.
asm :: GenAsm a => a -> T.Text
asm = asmToText . genAsm

-- | Generate an assembler prelude that starts the program
--   in the given function.
prelude :: Ident -> T.Text -> T.Text
prelude entrypoint code = T.unlines
  [ "\t.text"
  , "\t.global " <> entrypoint
  , "\t.type   " <> entrypoint <> ", @function"
  , "\t.p2align 4"
  , code
  , "\t.section .note.GNU-stack,\"\",@progbits"
  ]


-- * Turn AST nodes into assembly IR.

-- | Something that can generate assembly code for itself.
class GenAsm a where
  genAsm :: a -> NonEmpty.NonEmpty Asm

-- * Assembly generation for AST nodes.

instance GenAsm Program where
  genAsm (Program functions) = functions >>= genAsm

instance GenAsm Function where
  genAsm (Function ident statements) =
    ALabel ident <| (statements >>= genAsm)

instance GenAsm Statement where
  genAsm (Return expr) = genAsm expr <> instr ARet

instance GenAsm Expression where
  genAsm thisExpr = case thisExpr of
    (Constant i) -> instr $ APush i
    (Negate expr) -> genAsm expr <> instr ANegate
    (LogicNot expr) -> genAsm expr <> instr ALogicNot
    (BitNot expr) -> genAsm expr <> instr ABitNot
    (Add lhs rhs) -> genAsm lhs <> genAsm rhs <> instr AAdd
    (Sub lhs rhs) -> genAsm lhs <> genAsm rhs <> instr ASub
    (Mul lhs rhs) -> genAsm lhs <> genAsm rhs <> instr AMul
    (Div lhs rhs) -> genAsm lhs <> genAsm rhs <> instr ADiv
    (LogicAnd lhs rhs) -> genAsm lhs <> genAsm rhs <> instr ALogicAnd
    (LogicOr lhs rhs) -> genAsm lhs <> genAsm rhs <> instr ALogicOr

instr :: Asm -> NonEmpty.NonEmpty Asm
instr = NonEmpty.singleton


-- | Turn assembly IR into text.

asmToText :: NonEmpty.NonEmpty Asm -> T.Text
asmToText = accText . Prelude.foldl accAsmText emptyAcc . NonEmpty.toList

data AsmTextAcc = AsmTextAcc
  { accText :: T.Text
  , accNLabels :: Int
  }
  deriving (Show, Eq)

emptyAcc :: AsmTextAcc
emptyAcc = AsmTextAcc { accText = "", accNLabels = 0 }

nextLabel :: Int -> (T.Text, Int)
nextLabel i = (".L" <> showText i, i + 1)

showText :: Show a => a -> T.Text
showText = T.pack . show

accAsmText :: AsmTextAcc -> Asm -> AsmTextAcc
accAsmText acc asm = case asm of
  APush i ->
    let text = "\tpushq $" <> showText i <> "\n"
    in acc{ accText = acc.accText <> text }
  ALogicAnd ->
    let (falseLabel, n) = nextLabel acc.accNLabels
        (doneLabel, n') = nextLabel n
        text = T.unlines $ fInstrs
               [ "popq %rdx"
               , "popq %rax"
               , "cmpl $0, %eax"
               , "je " <> falseLabel
               , "cmpl $0, %edx"
               , "je " <> falseLabel
               , "movl $1, %eax"
               , "jmp " <> doneLabel
               ] <>
               fLabel falseLabel <>
               fInstr "movl $0, %eax" <>
               fLabel doneLabel <>
               fInstr "pushq %rax"
    in acc{ accText = acc.accText <> text, accNLabels = n' }
  ALogicOr ->
    let (trueLabel, n) = nextLabel acc.accNLabels
        (doneLabel, n') = nextLabel n
        text = T.unlines $ fInstrs
               [ "popq %rdx"
               , "popq %rax"
               , "cmpl $0, %eax"
               , "jne " <> trueLabel
               , "cmpl $0, %edx"
               , "jne " <> trueLabel
               , "movl $0, %eax"
               , "jmp " <> doneLabel
               ] <>
               fLabel trueLabel <>
               fInstr "movl $1, %eax" <>
               fLabel doneLabel <>
               fInstr "pushq %rax"
    in acc{ accText = acc.accText <> text, accNLabels = n' }
  AAdd ->
    let text = T.unlines $ fInstrs
               [ "popq %rdx", "popq %rax"
               , "addl %edx, %eax"
               , "pushq %rax" ]
    in acc{ accText = acc.accText <> text }
  ASub ->
    let text = T.unlines $ fInstrs
               [ "popq %rdx", "popq %rax"
               , "subl %edx, %eax"
               , "pushq %rax" ]
    in acc{ accText = acc.accText <> text }
  AMul ->
    let text = T.unlines $ fInstrs
               [ "popq %rdx", "popq %rax"
               , "imull %edx, %eax"
               , "pushq %rax" ]
    in acc{ accText = acc.accText <> text }
  ADiv ->
    let text = T.unlines $ fInstrs
               [ "popq %rbx", "popq %rax"
               , "cltd"
               , "idivl %ebx"
               , "pushq %rax" ]
    in acc{ accText = acc.accText <> text }
  ABitNot ->
    let text = T.unlines $ fInstrs
               [ "popq %rax"
               , "notl %eax"
               , "pushq %rax" ]
    in acc{ accText = acc.accText <> text }
  ALogicNot ->
    let text = T.unlines $ fInstrs
               [ "popq %rax"
               , "cmpl $0, %eax"
               , "sete %al"
               , "movzbl %al, %eax"
               , "pushq %rax" ]
    in acc{ accText = acc.accText <> text }
  ANegate ->
    let text = T.unlines $ fInstrs
               [ "popq %rax"
               , "neg %eax"
               , "pushq %rax" ]
    in acc{ accText = acc.accText <> text }
  ARet ->
    let text = T.unlines $ fInstrs
               [ "popq %rax"
               , "ret" ]
    in acc{ accText = acc.accText <> text }
  ALabel l ->
    let text = T.unlines $ fLabel l
    in acc{ accText = acc.accText <> text }


-- * Helpers for formatting assembly code.

fInstrs :: [T.Text] -> [T.Text]
fInstrs = Prelude.map (T.cons '\t')

fInstr :: T.Text -> [T.Text]
fInstr t = [T.cons '\t' t]

fLabel :: T.Text -> [T.Text]
fLabel t = [t <> ":"]
