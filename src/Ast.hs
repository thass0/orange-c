module Ast
  ( Ast
  , Program (..)
  , Function (..)
  , Ident
  , Statement (..)
  , Expression (..)
  ) where

import Data.List.NonEmpty
import Data.Text

type Ast = Program

data Program = Program (NonEmpty Function)
  deriving (Show, Eq)
-- There is no distinction here between a function
-- declaration and its definition. This distinction
-- purely made while parsing and is not reflected in
-- the AST.
data Function = Function Ident (NonEmpty Statement)
  deriving (Show, Eq)

type Ident = Text

data Statement = Return Expression
  deriving (Show, Eq)

data Expression
  = Negate Expression  -- ^ Unary arithmetic negation (-)
  | LogicNot Expression  -- ^ Unary logical negation (!)
  | BitNot Expression  -- ^ Unary bit-wise negation (~)
  | Constant Int  -- ^ Integer constant.
  deriving (Show, Eq)
