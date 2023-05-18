{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Main where

import Control.Applicative
import Control.Monad ()
import Data.Bits (Bits)
import qualified Data.Foldable as F
import Data.Functor (($>))
import qualified Data.Map as M
import Data.Void (Void)
import GHC.Generics (UInt)
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

type HackParser = Parsec Void String

aluMnemonicList =
  [ ("0", "0101010"),
    ("1", "0111111"),
    ("-1", "0111010"),
    ("!D", "0001101"),
    ("!A", "0110001"),
    ("!M", "1110001"),
    ("-D", "0001111"),
    ("-A", "0110011"),
    ("-M", "1110011"),
    ("D+1", "0011111"),
    ("A+1", "0110111"),
    ("M+1", "1110111"),
    ("D-1", "0001110"),
    ("A-1", "0110010"),
    ("M-1", "1110010"),
    ("D+A", "0000010"),
    ("D+M", "1000010"),
    ("D-A", "0010011"),
    ("D-M", "1010011"),
    ("A-D", "0000111"),
    ("M-D", "1000111"),
    ("D&A", "0000000"),
    ("D&M", "1000000"),
    ("D|A", "0010101"),
    ("D|M", "1010101"),
    -- ensure these three are last
    -- to avoid parser confusion
    ("D", "0001100"),
    ("A", "0110000"),
    ("M", "1110000")
  ]

aluMnemonicMap = M.fromList aluMnemonicList

aluMnemonic =
  map (M.string . fst) aluMnemonicList

storageMnemonicList =
  [ ("", "000"),
    ("ADM", "111"),
    ("DM", "011"),
    ("AM", "101"),
    ("AD", "110"),
    -- ensure these three are last
    -- to avoid parser confusion
    ("M", "001"),
    ("D", "010"),
    ("A", "100")
  ]

storageMnemonicMap = M.fromList storageMnemonicList

storageMnemonic =
  tail $ map (M.string . fst) storageMnemonicList

jumpMnemonicList =
  [ ("", "000"),
    ("JGT", "001"),
    ("JEQ", "010"),
    ("JGE", "011"),
    ("JLT", "100"),
    ("JNE", "101"),
    ("JLE", "110"),
    ("JMP", "111")
  ]

jumpMnemonicMap = M.fromList jumpMnemonicList

jumpMnemonic =
  tail $ map (M.string . fst) jumpMnemonicList

data Line
  = Blank
  | Comment
  | Label String
  | AInstrSymbol String
  | AInstrNum Int
  | CInstr (Maybe String)
  deriving (Show)

blank :: HackParser Line
blank = M.many M.hspace $> Blank

comment :: HackParser Line
comment = M.string "//" *> M.notFollowedBy M.newline $> Comment

cInstr :: HackParser Line
cInstr = do
  dest <- M.choice storageMnemonic <* M.char '=' <|> M.string ""
  comp <- M.choice aluMnemonic <|> M.string ""
  jump <- M.char ';' *> M.choice jumpMnemonic <|> M.string ""
  return $
    CInstr $
      F.fold
        [ Just "111",
          M.lookup comp aluMnemonicMap,
          M.lookup dest storageMnemonicMap,
          M.lookup jump jumpMnemonicMap
        ]

line :: HackParser Line
line = blank <|> comment

type HackProgram = [Line]

hackParse :: HackParser HackProgram
hackParse = M.sepBy1 line M.newline

parse :: String -> String
parse input = case M.runParser cInstr "" input of
  (Right s) -> show s
  (Left err) -> M.errorBundlePretty err

main :: IO ()
main = putStrLn $ parse "D=M+1;JMP"
