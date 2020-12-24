{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Translator where

import Control.Monad ( (>=>), forM_ )
import Control.Monad.Reader
  ( MonadReader (ask),
    Reader,
    forM_,
    runReader,
    (>=>),
  )
import Control.Monad.State
  ( MonadState (get, put),
    StateT (runStateT),
  )
import Control.Monad.Writer
  ( MonadWriter (tell),
    Writer,
    execWriter,
  )
import Data.Char (isSpace)
import qualified Data.List as DL
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as I
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)
import Data.Void (Void)
import System.Directory
import Text.Megaparsec
  ( MonadParsec (takeWhile1P),
    Parsec,
    choice,
    parse,
    (<|>),
  )
import Text.Megaparsec.Char (space, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Utils

type Parser = Parsec Void Text

data ArithmeticLogical
  = Add
  | Sub
  | Neg
  | Eq
  | Gt
  | Lt
  | And
  | Or
  | Not
  deriving (Show, Eq)

data Segment
  = Local
  | Argument
  | This
  | That
  | Constant
  | Static
  | Temp
  | Pointer
  deriving (Show, Eq)

data MemAccess
  = Pop Segment Integer
  | Push Segment Integer
  deriving (Show, Eq)

type Label = Text

data Branching
  = Goto Label
  | IfGoto Label
  | Label Label
  deriving (Show, Eq)

data Function
  = Call Label Integer
  | Function Label Integer
  | Return
  deriving (Show, Eq)

data Command
  = ArithCommand ArithmeticLogical
  | MemAccessCommand MemAccess
  | BranchingCommand Branching
  | FunctionCommand Function
  deriving (Show, Eq)

data CodegenInfo = CodegenInfo
  { labelId :: Integer,
    fileName :: String,
    funcName :: Text
  }
  deriving (Show, Eq)

type Env a = Reader CodegenInfo a

arithPairs :: [(ArithmeticLogical, Text)]
arithPairs =
  [ (Add, "add"),
    (Sub, "sub"),
    (Neg, "neg"),
    (Eq, "eq"),
    (Gt, "gt"),
    (Lt, "lt"),
    (And, "and"),
    (Or, "or"),
    (Not, "not")
  ]

segmentPairs :: [(Segment, Text)]
segmentPairs =
  [ (Local, "local"),
    (Argument, "argument"),
    (This, "this"),
    (That, "that"),
    (Constant, "constant"),
    (Static, "static"),
    (Temp, "temp"),
    (Pointer, "pointer")
  ]

unlinesConcise :: [Text] -> Text
unlinesConcise = T.stripEnd . T.unlines

pushD :: Text
pushD =
  unlinesConcise
    [ "@SP",
      "AM=M+1",
      "A=A-1",
      "M=D"
    ]

popD :: Text
popD =
  unlinesConcise
    [ "@SP",
      "AM=M-1",
      "D=M"
    ]

constD :: Integer -> Text
constD i = "@" <> toText i <> "\nD=A"

storeD :: Text -> Text
storeD s = "@" <> s <> "\nD=M"

storeM :: Text -> Text
storeM s = "@" <> s <> "\nM=D"

toText :: Integer -> Text
toText = T.pack . show

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

noCommentAndWS :: Char -> Bool
noCommentAndWS = not . ((&&) <$> (/= '/') <*> isSpace)

parseArith :: Parser ArithmeticLogical
parseArith = foldl1 (<|>) $ map (\(c, s) -> c <$ (string s :: Parser Text)) arithPairs

parseMem :: Parser MemAccess
parseMem = ((Push <$ lexeme (string "push")) <|> (Pop <$ lexeme (string "pop"))) <*> parseSegment <*> integer
  where
    parseSegment = foldl1 (<|>) $ map (\(sg, s) -> sg <$ lexeme (string s :: Parser Text)) segmentPairs
    integer = lexeme L.decimal :: Parser Integer

parseBranching :: Parser Branching
parseBranching =
  choice
    [ Label <$ lexeme (string "label") <*> parseToEnd,
      IfGoto <$ lexeme (string "if-goto") <*> parseToEnd,
      Goto <$ lexeme (string "goto") <*> parseToEnd
    ]
  where
    parseToEnd = takeWhile1P Nothing noCommentAndWS

parseFunction :: Parser Function
parseFunction =
  choice
    [ Call <$ lexeme (string "call") <*> lexeme (takeWhile1P Nothing (not . isSpace)) <*> lexeme L.decimal,
      Function <$ lexeme (string "function") <*> lexeme (takeWhile1P Nothing (not . isSpace)) <*> lexeme L.decimal,
      Return <$ lexeme (string "return")
    ]

compileArith :: ArithmeticLogical -> Env Text
compileArith cmd = do
  info <- ask
  return $ case cmd of
    Add -> compileBin "M=M+D"
    Sub -> compileBin "M=M-D"
    Neg -> compileUna "M=-M"
    Eq -> compileCmp "JEQ" info
    Gt -> compileCmp "JGT" info
    Lt -> compileCmp "JLT" info
    And -> compileBin "M=D&M"
    Or -> compileBin "M=D|M"
    Not -> compileUna "M=!M"
  where
    topStack = unlinesConcise ["@SP", "A=M-1"]
    compileBin s = unlinesConcise [popD, "A=A-1", s]
    compileUna s = unlinesConcise [topStack, s]
    compileCmp op info =
      unlinesConcise
        [ popD,
          "A=A-1",
          "D=M-D",
          "M=-1",
          "@END_" <> op <> "_" <> (toText . labelId) info,
          "D;" <> op,
          topStack,
          "M=0",
          "(END_" <> op <> "_" <> (toText . labelId) info <> ")"
        ]

compileMem :: MemAccess -> Env Text
compileMem cmd = do
  info <- ask
  return $ case cmd of
    Push Constant i -> thenPushD (constD i)
    Push Static i -> thenPushD . storeD $ T.pack (fileName info) <> "." <> toText i
    Push Temp i -> thenPushD . storeD $ toText (5 + i)
    Push Pointer i -> thenPushD . storeD $ if i == 0 then "THIS" else "THAT"
    Push segment i -> thenPushD $ unlinesConcise [constD i, "@" <> compileSeg segment, "A=M+D", "D=M"]
    Pop Static i -> popDThen $ storeM (T.pack (fileName info) <> "." <> toText i)
    Pop Temp i -> popDThen . storeM $ toText (5 + i)
    Pop Pointer i -> popDThen . storeM $ if i == 0 then "THIS" else "THAT"
    Pop segment i -> unlinesConcise [constD i, "@" <> compileSeg segment, "D=D+M", "@SP", "AM=M-1", "D=D+M", "A=D-M", "M=D-A"]
  where
    thenPushD c = unlinesConcise [c, pushD]
    popDThen c = unlinesConcise [popD, c]
    compileSeg = \case
      Local -> "LCL"
      Argument -> "ARG"
      This -> "THIS"
      That -> "THAT"

compileBranching :: Branching -> Env Text
compileBranching cmd = do
  info <- ask
  return $ case cmd of
    Goto label -> unlinesConcise ["@" <> funcName info <> label, "0;JMP"]
    IfGoto label -> unlinesConcise [popD, "@" <> funcName info <> label, "D;JNE"]
    Label label -> "(" <> funcName info <> label <> ")"

compileFunction :: Function -> Env Text
compileFunction (Function name nVars) = do
  info <- ask
  return $
    unlinesConcise
      [ -- Generate label
        "(" <> name <> ")",
        -- Initialize local
        "D=0",
        T.replicate (fromIntegral nVars) (pushD <> "\n")
      ]
compileFunction (Call fname nArgs) = do
  info <- ask
  return $
    unlinesConcise
      [ -- Save return address
        "@" <> funcName info <> "ret." <> toText (labelId info),
        "D=A",
        pushD,
        -- Save call stack
        save "LCL",
        save "ARG",
        save "THIS",
        save "THAT",
        -- Reposition ARG
        constD (5 + nArgs),
        "@SP",
        "D=M-D",
        storeM "ARG",
        -- Reposition LCL
        storeD "SP",
        storeM "LCL",
        -- Goto
        "@" <> fname,
        "0;JMP",
        -- Create return address
        "(" <> funcName info <> "ret." <> toText (labelId info) <> ")"
      ]
  where
    save reg = unlinesConcise [storeD reg, pushD]
compileFunction Return = do
  info <- ask
  return $
    unlinesConcise
      [ -- nArgs = 0 will cause a trouble, need to save the return address in advance
        constD 5,
        "@LCL",
        "A=M-D",
        "D=M",
        storeM "R14",
        -- Reposition return value for the caller
        popD,
        "@ARG",
        "A=M",
        "M=D",
        -- Reposition SP = ARG + 1
        "D=A+1",
        storeM "SP",
        -- endFrame = LCL
        storeD "LCL",
        storeM "R13",
        -- Restore one by one
        restore "THAT",
        restore "THIS",
        restore "ARG",
        restore "LCL",
        -- Go to caller's return address
        "@R14",
        "A=M",
        "0;JMP"
      ]
  where
    restore seg =
      unlinesConcise
        [ "@R13",
          "AM=M-1",
          "D=M",
          storeM seg
        ]

parseCommand :: Parser Command
parseCommand =
  choice
    [ ArithCommand <$> parseArith,
      MemAccessCommand <$> parseMem,
      BranchingCommand <$> parseBranching,
      FunctionCommand <$> parseFunction
    ]

canonicalize :: [Text] -> [Text]
canonicalize = filter (not . shouldIgnore) . map strip
  where
    strip = T.strip . T.takeWhile (/= '/')
    shouldIgnore = (||) <$> T.null <*> T.isPrefixOf "//"

translateVM :: String -> Text -> Text
translateVM fname s = toStrict . toLazyText $ execWriter (runStateT (go src) env)
  where
    env = CodegenInfo {labelId = 0, fileName = fname, funcName = ""}
    src = canonicalize $ T.lines s
    go :: [Text] -> StateT CodegenInfo (Writer Builder) ()
    go lines = forM_ lines $ \line -> do
      info <- get
      tell . fromText $ "// " <> line <> "\n"

      let Right cmd = parse parseCommand fname line

      case cmd of
        FunctionCommand f@(Function name _) -> save (compileFunction f) info {funcName = name <> "$"}
        FunctionCommand c@(Call _ _) -> save (compileFunction c) info {labelId = labelId info + 1}
        FunctionCommand f -> save (compileFunction f) info
        ArithCommand a -> save (compileArith a) info {labelId = labelId info + if a `elem` [Eq, Gt, Lt] then 1 else 0}
        BranchingCommand b -> save (compileBranching b) info
        MemAccessCommand m -> save (compileMem m) info
      where
        save :: Env Text -> CodegenInfo -> StateT CodegenInfo (Writer Builder) ()
        save e info = tell (fromText (runReader e info) <> "\n") >> put info

initVM :: String -> Text
initVM fname =
  T.unlines
    [ -- SP = 0
      "@256",
      "D=A",
      "@SP",
      "M=D",
      -- Call Sys.init
      runReader (compileFunction (Call "Sys.init" 0)) CodegenInfo {labelId = 0, funcName = T.pack (fname <> "$"), fileName = ""}
    ]

compileFolder :: IO ()
compileFolder = do
  putStr "Path = "
  path <- getLine
  isFile <- doesFileExist path

  if isFile
    then I.readFile >=> I.writeFile (combinePath (parentDir path) (baseName path <> ".asm")) . translateVM (baseName path) $ path
    else do
      let fileName = combinePath path (baseName path <> ".asm")
      I.writeFile fileName $ initVM (baseName path)

      files <- map (combinePath path) . filter (DL.isSuffixOf ".vm") <$> listDirectory path
      forM_ files $ \file -> I.readFile >=> I.appendFile fileName . translateVM (baseName file) $ file