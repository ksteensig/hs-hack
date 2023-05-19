{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Parser where

import Control.Applicative
import qualified Data.Foldable as F
import Data.Functor (($>))
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Void (Void)
import Numeric (readInt)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

type HackParser = M.Parsec Void String

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

blank :: HackParser Line
blank = M.many M.hspace1 *> M.newline *> M.many M.hspace1 $> Blank

comment :: HackParser Line
comment = do
  _ <- M.string "//"
  c <- M.takeWhile1P Nothing (/= '\n')
  return $ Comment c

label :: HackParser Line
label = do
  _ <- M.char '('
  l <- M.letterChar
  ls <- M.many M.letterChar
  _ <- M.char ')'
  c <-
    fmap
      (\(Comment c') -> Just c')
      (M.many M.hspace1 *> comment)
      <|> pure Nothing
  return $ Label (l : ls) c

aInstrNum :: HackParser Line
aInstrNum = do
  _ <- M.char '@'
  n <- M.digitChar
  ns <- M.many M.digitChar
  c <-
    fmap
      (\(Comment c') -> Just c')
      (M.many M.hspace1 *> comment)
      <|> pure Nothing
  return $ AInstrNum ((\n -> read n :: Int) (n : ns)) c

aInstrSymbol :: HackParser Line
aInstrSymbol = do
  _ <- M.char '@'
  s <- M.letterChar
  ss <- M.many M.alphaNumChar
  c <-
    fmap
      (\(Comment c') -> Just c')
      (M.many M.hspace1 *> comment)
      <|> pure Nothing
  return $ AInstrSymbol (s : ss) c

cInstr :: HackParser Line
cInstr = do
  dest <-
    M.try
      ( M.choice storageMnemonic
          <* M.char '='
      )
      <|> M.string ""
  comp <- M.choice aluMnemonic
  jump <-
    M.try
      ( M.char ';'
          *> M.choice jumpMnemonic
      )
      <|> M.string ""
  c <-
    fmap
      (\(Comment c') -> Just c')
      (M.many M.hspace1 *> comment)
      <|> pure Nothing
  return $ CInstr dest comp jump c

-- fromJust $ -- bad practice but input is assumed correct
--   F.fold
--     [ Just "111",
--       M.lookup comp aluMnemonicMap,
--       M.lookup dest storageMnemonicMap,
--       M.lookup jump jumpMnemonicMap
--     ]

line :: HackParser Line
line =
  M.choice
    ( fmap
        M.try -- look ahead is needed
        [ blank,
          label,
          comment,
          aInstrNum,
          aInstrSymbol,
          cInstr
        ]
    )
    <* M.optional M.newline

type HackProgram = [Line]

hackParse :: HackParser HackProgram
hackParse = M.many (M.many M.hspace1 *> line) <* M.optional M.newline <* M.eof