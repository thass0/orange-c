module Codegen
  ( asm
  , prelude
  , GenAsm (..)
  , fInstr
  , fInstrs
  , fInstrs'
  , fLabel
  , fLabel'
  ) where

import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty ((<|))
import qualified Data.Text as T

import Ast

-- | A intermediate representation of operations expressed
--   in terms of assembler code.
data Asm
  = APush Int
  | ARet
  | ANegate
  | AAdd
  | ASub
  | AMul
  | ADiv
  | AMod
  | ALeftShift
  | ARightShift
  | ALogicAnd
  | ALogicOr
  | ALogicNot
  | ABitAnd
  | ABitOr
  | ABitNot
  | ABitXor
  | ARelEq
  | ARelNeq
  | ARelLeq
  | ARelL
  | ARelGeq
  | ARelG
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
    (Add lhs rhs) -> genAsm lhs <> genAsm rhs <> instr AAdd
    (Sub lhs rhs) -> genAsm lhs <> genAsm rhs <> instr ASub
    (Mul lhs rhs) -> genAsm lhs <> genAsm rhs <> instr AMul
    (Div lhs rhs) -> genAsm lhs <> genAsm rhs <> instr ADiv
    (Mod lhs rhs) -> genAsm lhs <> genAsm rhs <> instr AMod
    (LeftShift lhs rhs) -> genAsm lhs <> genAsm rhs <> instr ALeftShift
    (RightShift lhs rhs) -> genAsm lhs <> genAsm rhs <> instr ARightShift
    (LogicAnd lhs rhs) -> genAsm lhs <> genAsm rhs <> instr ALogicAnd
    (LogicOr lhs rhs) -> genAsm lhs <> genAsm rhs <> instr ALogicOr
    (LogicNot expr) -> genAsm expr <> instr ALogicNot
    (BitAnd lhs rhs) -> genAsm lhs <> genAsm rhs <> instr ABitAnd
    (BitOr lhs rhs) -> genAsm lhs <> genAsm rhs <> instr ABitOr
    (BitNot expr) -> genAsm expr <> instr ABitNot
    (BitXor lhs rhs) -> genAsm lhs <> genAsm rhs <> instr ABitXor
    (RelEq lhs rhs) -> genAsm lhs <> genAsm rhs <> instr ARelEq
    (RelNeq lhs rhs) -> genAsm lhs <> genAsm rhs <> instr ARelNeq
    (RelLeq lhs rhs) -> genAsm lhs <> genAsm rhs <> instr ARelLeq
    (RelL lhs rhs) -> genAsm lhs <> genAsm rhs <> instr ARelL
    (RelGeq lhs rhs) -> genAsm lhs <> genAsm rhs <> instr ARelGeq
    (RelG lhs rhs) -> genAsm lhs <> genAsm rhs <> instr ARelG

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
accAsmText acc asmCode = case asmCode of
  APush i -> appendText $ "\tpushq $" <> showText i <> "\n"
  ANegate -> appendText $ fInstrs
             [ "popq %rax"
             , "neg %eax"
             , "pushq %rax" ]
  ARet -> appendText $ fInstrs
          [ "popq %rax"
          , "ret" ]
  AAdd -> appendText $ fInstrs
          [ "popq %rdx", "popq %rax"
          , "addl %edx, %eax"
          , "pushq %rax" ]
  ASub -> appendText $ fInstrs
          [ "popq %rdx", "popq %rax"
          , "subl %edx, %eax"
          , "pushq %rax" ]
  AMul -> appendText $ fInstrs
          [ "popq %rdx", "popq %rax"
          , "imull %edx, %eax"
          , "pushq %rax" ]
  ADiv -> appendText $ fInstrs
          [ "popq %rbx", "popq %rax"
          , "cltd"
          , "idivl %ebx"
          , "pushq %rax" ]
  AMod -> undefined
  ALeftShift -> undefined
  ARightShift -> undefined
  ALogicAnd ->
    let (falseLabel, n) = nextLabel acc.accNLabels
        (doneLabel, n') = nextLabel n
        text = T.unlines $ fInstrs'
               [ "popq %rdx"
               , "popq %rax"
               , "cmpl $0, %eax"
               , "je " <> falseLabel
               , "cmpl $0, %edx"
               , "je " <> falseLabel
               , "movl $1, %eax"
               , "jmp " <> doneLabel
               ] <>
               fLabel' falseLabel <>
               fInstr "movl $0, %eax" <>
               fLabel' doneLabel <>
               fInstr "pushq %rax"
    in acc{ accText = acc.accText <> text, accNLabels = n' }
  ALogicOr ->
    let (trueLabel, n) = nextLabel acc.accNLabels
        (doneLabel, n') = nextLabel n
        text = T.unlines $ fInstrs'
               [ "popq %rdx"
               , "popq %rax"
               , "cmpl $0, %eax"
               , "jne " <> trueLabel
               , "cmpl $0, %edx"
               , "jne " <> trueLabel
               , "movl $0, %eax"
               , "jmp " <> doneLabel
               ] <>
               fLabel' trueLabel <>
               fInstr "movl $1, %eax" <>
               fLabel' doneLabel <>
               fInstr "pushq %rax"
    in acc{ accText = acc.accText <> text, accNLabels = n' }
  ALogicNot -> appendText $ fInstrs
               [ "popq %rax"
               , "cmpl $0, %eax"
               , "sete %al"
               , "movzbl %al, %eax"
               , "pushq %rax" ]
  ABitAnd -> undefined
  ABitOr -> undefined
  ABitNot -> appendText $ fInstrs
             [ "popq %rax"
             , "notl %eax"
             , "pushq %rax" ]
  ABitXor -> undefined
  ARelEq -> undefined
  ARelNeq -> undefined
  ARelLeq -> undefined
  ARelL -> undefined
  ARelGeq -> undefined
  ARelG -> undefined
  ALabel l -> appendText $ fLabel l <> "\n"
  where
    appendText :: T.Text -> AsmTextAcc
    appendText newText =
      acc{ accText = acc.accText <> newText }

-- * Helpers for formatting assembly code.

fInstrs :: [T.Text] -> T.Text
fInstrs = T.unlines . fInstrs'

fInstrs' :: [T.Text] -> [T.Text]
fInstrs' = Prelude.map (T.cons '\t')

fInstr :: T.Text -> [T.Text]
fInstr t = [T.cons '\t' t]

fLabel :: T.Text -> T.Text
fLabel = (<> ":")

fLabel' :: T.Text -> [T.Text]
fLabel' t = [t <> ":"]

