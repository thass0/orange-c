module Codegen (asm, prelude, Asm) where

import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty ((<|))
import qualified Data.Text as T

import Parse

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
  genAsm (Program functions) =
    -- NonEmpty.fromList (concat (map NonEmpty.toList (map genAsm (NonEmpty.toList functions))))
    functions >>= genAsm

instance GenAsm Function where
  genAsm (Function ident statements) =
    -- (id <> ":") `NonEmpty.cons` (NonEmpty.fromList (concat (map NonEmpty.toList (map genAsm (NonEmpty.toList statements)))))
    (ident <> ":") <| (statements >>= genAsm)

instance GenAsm Statement where
  genAsm (Return expression) =
    genAsm expression <> (NonEmpty.singleton $ T.pack "\tret")

instance GenAsm Expression where
  genAsm (Constant i) = NonEmpty.singleton $ T.pack $
    "\tmovl $" <> show i <> ", %eax"
