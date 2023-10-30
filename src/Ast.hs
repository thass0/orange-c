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
  | Add Expression Expression  -- ^ Addition (+)
  | Sub Expression Expression  -- ^ Subtraction (-)
  | Mul Expression Expression  -- ^ Multiplication (*)
  | Div Expression Expression  -- ^ Division (/)
  | Mod Expression Expression  -- ^ Modulo (%)
  | LeftShift Expression Expression  -- ^ Bit-wise left shift (<<)
  | RightShift Expression Expression  -- ^ Bit-wise right shift (>>)
  | LogicAnd Expression Expression  -- ^ Logical AND (&&)
  | LogicOr Expression Expression  -- ^ Logical OR (||)
  | LogicNot Expression  -- ^ Unary logical negation (!)
  | BitAnd Expression Expression  -- ^ Bit-wise AND (&)
  | BitOr Expression Expression  -- ^ Bit-wise OR (|)
  | BitNot Expression  -- ^ Unary bit-wise negation (~)
  | BitXor Expression Expression  -- ^ Bit-wise XOR (^)
  | RelEq Expression Expression  -- ^ Equal (==)
  | RelNeq Expression Expression  -- ^ Not equal (!=)
  | RelLeq Expression Expression  -- ^ Less or equal (<=)
  | RelL Expression Expression  -- ^ Less (<)
  | RelGeq Expression Expression  -- ^ Greater or equal (>=)
  | RelG Expression Expression  -- ^ Greater (>)
  | Constant Int  -- ^ Integer constant.
  deriving (Show, Eq)
