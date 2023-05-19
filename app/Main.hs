module Main where

import qualified Data.Foldable as F
import Parser
import qualified Text.Megaparsec as M

main :: IO ()
main = do
  input <- readFile "test.hack"
  case M.runParser hackParse "" input of
    (Right s) -> writeFile "output.asm" $ F.foldl' (++) "" (map ((++ "\n") . show) s)
    (Left err) -> putStr $ M.errorBundlePretty err
