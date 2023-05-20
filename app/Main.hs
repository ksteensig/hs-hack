module Main where

import Assembler
import Ast
import qualified Data.Foldable as F
import Data.Word (Word16)
import Parser
import qualified Text.Megaparsec as M

main :: IO ()
main = do
  input <- readFile "test.asm"
  case M.runParser hackParse "" input of
    (Right s) -> writeFile "output.hack" $ F.foldl' (++) "" (map (++ "\n") $ translate s)
    (Left err) -> putStr $ M.errorBundlePretty err
