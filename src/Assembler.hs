{-# LANGUAGE OverloadedStrings #-}

module Assembler (compile) where

import Data.Char (isDigit)
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)
import Data.Text.Read (decimal)
import Data.Void (Void)
import Numeric (showIntAtBase)
import Text.Megaparsec
  (takeRest,  MonadParsec (eof, takeWhile1P, takeWhileP, try),
    Parsec,
    option,
    optional,
    parse,
    parseMaybe,
    some,
    (<|>),
  )
import Text.Megaparsec.Char (char, digitChar, letterChar, space)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data AIns
  = Const Integer
  | Label Text
  deriving (Eq, Show)

type Dest = Text

type Comp = Text

type Jump = Text

data CIns
  = CIns Dest Comp Jump
  deriving (Eq, Show)

data CompilationData = CompilationData
  { symbolTable :: M.Map Text Integer,
    output :: Builder,
    allocatedAddr :: Integer
  }
  deriving (Show)

-- Easier to write but unefficient
compCodes :: M.Map Text Text
compCodes = M.fromList $ map (\(a, b, c) -> (a, T.pack (show b) <> (cinsCodes !! fromInteger c))) compMap
  where
    compMap =
      [ ("0", 0, 0),
        ("1", 0, 1),
        ("-1", 0, 2),
        ("D", 0, 3),
        ("A", 0, 4),
        ("M", 1, 4),
        ("!D", 0, 5),
        ("!A", 0, 6),
        ("!M", 1, 6),
        ("-D", 0, 7),
        ("-A", 0, 8),
        ("D+1", 0, 9),
        ("A+1", 0, 10),
        ("M+1", 1, 10),
        ("D-1", 0, 11),
        ("A-1", 0, 12),
        ("M-1", 1, 12),
        ("D+A", 0, 13),
        ("D+M", 1, 13),
        ("D-A", 0, 14),
        ("D-M", 1, 14),
        ("A-D", 0, 15),
        ("M-D", 1, 15),
        ("D&A", 0, 16),
        ("D&M", 1, 16),
        ("D|A", 0, 17),
        ("D|M", 1, 17)
      ]
    cinsCodes =
      [ "101010",
        "111111",
        "111010",
        "001100",
        "110000",
        "001101",
        "110001",
        "001111",
        "110011",
        "011111",
        "110111",
        "001110",
        "110010",
        "000010",
        "010011",
        "000111",
        "000000",
        "010101"
      ]

destCodes :: M.Map Dest Text
destCodes =
  M.fromList
    [ ("", "000"),
      ("M", "001"),
      ("D", "010"),
      ("MD", "011"),
      ("A", "100"),
      ("AM", "101"),
      ("AD", "110"),
      ("AMD", "111")
    ]

jumpCodes :: M.Map Text Text
jumpCodes =
  M.fromList
    [ ("", "000"),
      ("JGT", "001"),
      ("JEQ", "010"),
      ("JGE", "011"),
      ("JLT", "100"),
      ("JNE", "101"),
      ("JLE", "110"),
      ("JMP", "111")
    ]

canonicalize :: [Text] -> [Text]
canonicalize = filter (not . shouldIgnore) . map strip
  where
    strip = T.strip . T.takeWhile (/= '/')
    shouldIgnore = (||) <$> T.null <*> T.isPrefixOf "//"

collectLabel :: [Text] -> M.Map Text Integer
collectLabel lines = foldl' folder M.empty (zip [0 ..] lines)
  where
    folder m x =
      if "(" `T.isPrefixOf` snd x
        then M.insert (strip . snd $ x) (fst x - fromIntegral (M.size m)) m
        else m
    strip = T.init . T.tail

defaultSymbolTable :: M.Map Text Integer
defaultSymbolTable =
  M.fromList
    [ ("R0", 0),
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
      ("SCREEN", 16384),
      ("KBD", 24576),
      ("SP", 0),
      ("LCL", 1),
      ("ARG", 2),
      ("THIS", 3),
      ("THAT", 4),
      ("LOOP", 4),
      ("STOP", 18),
      ("END", 22)
    ]

to16BinString :: Integer -> Text
to16BinString x = T.pack $ padZero16 $ showIntAtBase 2 digitToChar x ""
  where
    digitToChar x = toEnum (fromEnum '0' + x) :: Char
    padZero16 = \s -> replicate (16 - length s) '0' ++ s

parseA' :: Text -> AIns
parseA' s = let Right n = parse (char '@' >> try (Const <$> integer <* eof) <|> Label <$> letter) "" s in n
  where
    integer = L.lexeme space L.decimal :: Parser Integer
    letter = T.pack <$> some (letterChar <|> digitChar)

parseC' :: Text -> CIns
parseC' = fromJust . parseMaybe go
  where
    go :: Parser CIns
    go = do
      dest <- option "" (try $ takeWhile1P Nothing (/= '=') <* char '=')
      comp <- takeWhile1P Nothing (/= ';') <* optional (char ';')
      CIns dest comp <$> takeRest 

parseA :: Text -> AIns
parseA s
  | (not . T.null . digitOnly . T.tail) s = Const $ (toInt . T.tail) s
  | otherwise = Label (T.tail s)
  where
    digitOnly = T.takeWhile isDigit
    toInt s = let Right (n, _) = decimal s in n

parseC :: Text -> CIns
parseC s = CIns (fst dest) (T.dropWhile (== '=') (fst comp)) jump
  where
    extract :: (Text, Text) -> (Text, Text)
    extract (x, "") = ("", x)
    extract x = x

    dest = extract $ T.breakOn "=" s
    comp = T.breakOn ";" (snd dest)
    jump = T.dropWhile (== ';') (snd comp)

compileA :: AIns -> CompilationData -> CompilationData
compileA (Const num) dat = dat {output = fromText $ to16BinString num}
compileA (Label str) dat =
  fromMaybe
    dat
      { output = fromText $ to16BinString (allocatedAddr dat),
        allocatedAddr = allocatedAddr dat + 1,
        symbolTable = M.insert str (allocatedAddr dat) (symbolTable dat)
      }
    $ do
      label <- M.lookup str (symbolTable dat)
      return dat {output = fromText $ to16BinString label}

compileC :: CIns -> CompilationData -> CompilationData
compileC (CIns dest comp jump) dat = fromMaybe dat $ do
  d <- M.lookup dest destCodes
  c <- M.lookup comp compCodes
  j <- M.lookup jump jumpCodes
  return dat {output = fromText $ mconcat ["111", c, d, j]}

compile :: Text -> Text
compile s =
  toStrict . toLazyText . output $
      foldl
        folder
        CompilationData
          { symbolTable = M.union defaultSymbolTable (collectLabel src),
            output = "",
            allocatedAddr = 16
          }
        src
  where
    src = canonicalize $ T.lines s
    folder acc x =
      if "(" `T.isPrefixOf` x
        then acc
        else
          let res = if "@" `T.isPrefixOf` x then compileA (parseA' x) acc else compileC (parseC' x) acc
           in res {output = output acc <> output res <> "\n"}