module OrangeC
    ( compile
    ) where

import Data.Text

import qualified Codegen as Gen
import Parse

compile :: FilePath -> Text -> Either String Text
compile file src = do
  ast <- parse file src
  let asm = Gen.asm ast
  Right $ Gen.prelude "main" asm
