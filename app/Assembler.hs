module Assembler where

import Ast
import Data.Char (intToDigit)
import Data.Foldable (fold)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Numeric (showIntAtBase)

aluMnemonicMap = M.fromList aluMnemonicList

storageMnemonicMap = M.fromList storageMnemonicList

jumpMnemonicMap = M.fromList jumpMnemonicList

type LineBits = String

type HackProgramBits = [LineBits]

isBlank Blank = False
isBlank _ = True

isComment (Comment _) = False
isComment _ = True

isLabel (Label _ _) = False
isLabel _ = True

type SymbolTable = (M.Map String Int)

initTable =
  M.fromList
    [ ("SP", 0),
      ("LCL", 1),
      ("ARG", 2),
      ("THIS", 3),
      ("THAT", 4),
      ("R0", 0),
      ("R1", 1),
      ("R2", 2),
      ("R3", 3),
      ("R4", 4),
      ("R5", 5),
      ("R6", 6),
      ("R7", 7),
      ("R8", 8),
      ("R9", 9),
      ("R10", 10),
      ("R11", 11),
      ("R12", 12),
      ("R13", 13),
      ("R14", 14),
      ("R15", 15),
      ("SCREEN", 0x4000),
      ("KBD", 0x6000)
    ]

varStartAddr = 0x10

newtype AsmState = AsmState SymbolTable

replaceVariables pos table [] = table
replaceVariables pos table ((AInstrSymbol s _) : ls) = case addr of
  Nothing -> replaceVariables (pos + 1) table' ls
  (Just _) -> replaceVariables pos table ls
  where
    addr = M.lookup s table
    table' = M.insert s pos table
replaceVariables pos table (l : ls) = replaceVariables pos table ls

replaceLabels _ table [] = table
replaceLabels pos table ((Label l _) : ls) = case addr of
  Nothing -> replaceLabels pos table' ls
  (Just _) -> replaceLabels pos table ls
  where
    addr = M.lookup l table
    table' = M.insert l pos table
replaceLabels pos table (l : ls) = replaceLabels (pos + 1) table ls

translateLine :: (Integral a, Show a) => M.Map String a -> Line -> [Char]
translateLine table (AInstrNum n _) = n'' ++ n'
  where
    n' = showIntAtBase 2 intToDigit n ""
    n'' = replicate (16 - length n') '0'
translateLine table (AInstrSymbol s _) = n'' ++ n'
  where
    n = fromJust $ M.lookup s table
    n' = showIntAtBase 2 intToDigit n ""
    n'' = replicate (16 - length n') '0'
translateLine table (CInstr dest comp jump _) =
  fromJust $ -- bad practice but input is assumed correct
    fold
      [ Just "111",
        M.lookup comp aluMnemonicMap,
        M.lookup dest storageMnemonicMap,
        M.lookup jump jumpMnemonicMap
      ]

-- translate :: HackProgram -> [String]
translate prog = map (translateLine table'') prog''
  where
    prog' = (filter isComment . filter isBlank) prog
    table' = replaceLabels 0 initTable prog'
    table'' = replaceVariables varStartAddr table' prog'
    prog'' = filter isLabel prog'
