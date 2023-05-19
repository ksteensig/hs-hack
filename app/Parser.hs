module Parser where

import Ast
import Control.Applicative
import qualified Data.Foldable as F
import Data.Functor (($>))
import Data.Maybe (fromJust)
import Data.Void (Void)
import Numeric (readInt)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

type HackParser = M.Parsec Void String

aluMnemonic =
  map (M.string . fst) aluMnemonicList

storageMnemonic =
  tail $ map (M.string . fst) storageMnemonicList

jumpMnemonic =
  tail $ map (M.string . fst) jumpMnemonicList

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

hackParse :: HackParser HackProgram
hackParse = M.many (M.many M.hspace1 *> line) <* M.optional M.newline <* M.eof