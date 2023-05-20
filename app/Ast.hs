module Ast where

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
    ("A+D", "0000010"),
    ("D+M", "1000010"),
    ("M+D", "1000010"),
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

type Comment = String

data Line
  = Blank
  | Comment Comment
  | Label String (Maybe Comment)
  | AInstrSymbol String (Maybe Comment)
  | AInstrNum Int (Maybe Comment)
  | CInstr String String String (Maybe Comment)

instance Show Line where
  show Blank = ""
  show (Comment c) = "//" ++ c
  show (Label l (Just c)) = "(" ++ l ++ ")" ++ " //" ++ c
  show (Label l Nothing) = "(" ++ l ++ ")"
  show (AInstrSymbol s (Just c)) = "@" ++ s ++ " //" ++ c
  show (AInstrSymbol s Nothing) = "@" ++ s
  show (AInstrNum n (Just c)) = "@" ++ show n ++ " //" ++ c
  show (AInstrNum n Nothing) = "@" ++ show n
  show (CInstr dest comp jump (Just c)) =
    show (CInstr dest comp jump Nothing)
      ++ " //"
      ++ c
  show (CInstr dest comp jump Nothing) =
    ( if dest /= ""
        then dest ++ "="
        else ""
    )
      ++ comp
      ++ ( if jump /= ""
             then ";" ++ jump
             else ""
         )

type HackProgram = [Line]