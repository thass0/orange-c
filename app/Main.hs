module Main (main) where

import qualified Data.Text.IO as TextIO
import System.Environment
import System.Exit

import OrangeC

main :: IO ()
main = do
  args <- getArgs
  let filename = case args of
        [f] -> f
        _ -> "<stdin>"
  contents <- TextIO.getContents
  case compile filename contents of
    Right asm -> do
      TextIO.putStr asm
      exitSuccess
    Left err -> do
      putStr err
      exitFailure
