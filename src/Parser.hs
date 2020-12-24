{-# LANGUAGE OverloadedStrings #-}

module Parser (parseProgram) where

import Control.Applicative (liftA2, liftA3)
import Control.Monad.Combinators.Expr
    ( makeExprParser, Operator(InfixL, Prefix, Postfix) )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
  (MonadParsec(try), parse,  Parsec,
    between,
    choice,
    many,
    manyTill,
    option,
    optional,
    (<|>),
    parseTest
  )
import Text.Megaparsec.Char
    ( char, digitChar, letterChar, space1, string )
import qualified Text.Megaparsec.Char.Lexer as L
import Types
import Text.Megaparsec.Error (ParseErrorBundle)

type Parser = Parsec Void String

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" (Unary UnaryDash),
      prefix "~" (Unary UnaryTilde),
      prefix "+" id
    ],
    [ binary "*" (Binary Star),
      binary "/" (Binary Slash)
    ],
    [ binary "+" (Binary Plus),
      binary "-" (Binary Minus)
    ],
    [ binary "<" (Binary LessThan),
      binary ">" (Binary GreaterThan),
      binary "=" (Binary Equal)
    ],
    [ binary "&" (Binary Ampersand),
      binary "|" (Binary Pipe)
    ]
  ]
  where
    prefix, postfix :: String -> (Expr -> Expr) -> Operator Parser Expr
    prefix name f = Prefix (f <$ symbol name)
    postfix name f = Postfix (f <$ symbol name)
    binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
    binary name f = InfixL (f <$ symbol name)

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/") 

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol = L.symbol sc

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- Expressions

int :: Parser Expr
int = Int <$> lexeme L.decimal

str :: Parser Expr
str = String <$> lexeme (char '"' *> manyTill L.charLiteral (symbol "\""))

identifier :: Parser String
identifier = lexeme $ (:) <$> (letterChar <|> char '_') <*> many (letterChar <|> digitChar <|> char '_')

var :: Parser Expr
var = lexeme $ liftA2 Var identifier $ optional (brackets expr)

call :: Parser Expr
call = lexeme $ liftA3 Call (optional . try $ identifier <* symbol ".") identifier $ parens exprList
  where
    exprList = option [] $ (:) <$> expr <*> many (symbol "," *> expr)

term :: Parser Expr
term =
  choice
    [ int,
      str,
      keywordConst,
      try call,
      var,
      parens expr
    ]
  where
    keywordConst = lexeme $ foldl1 (<|>) $ map (\(s, k) -> Keyword k <$ string s) keywordConsts

expr :: Parser Expr
expr = makeExprParser term operatorTable

-- Statements

letStmt :: Parser Stmt
letStmt = symbol "let" >> liftA3 Let identifier (optional $ brackets expr) (between (symbol "=") (symbol ";") expr)

ifStmt :: Parser Stmt
ifStmt = If <$ symbol "if" <*> parens expr <*> braces (many stmt) <*> optional (symbol "else" *> braces (many stmt))

whileStmt :: Parser Stmt
whileStmt = While <$ symbol "while" <*> parens expr <*> braces (many stmt)

doStmt :: Parser Stmt
doStmt = Do <$> between (symbol "do") (symbol ";") call

returnStmt :: Parser Stmt
returnStmt = Return <$> between (symbol "return") (symbol ";") (optional expr)

stmt :: Parser Stmt
stmt =
  choice
    [ letStmt,
      ifStmt,
      whileStmt,
      doStmt,
      returnStmt
    ]

-- Program structure

varDec :: Parser SubroutineVarDec
varDec =
  lexeme $
    symbol "var"
      *> liftA2 SubroutineVarDec type' (liftA2 (:) identifier $ many (symbol "," >> identifier)) <* symbol ";"

subroutineDec :: Parser SubroutineDec
subroutineDec = do
  decType <- lexeme $ Constructor <$ symbol "constructor" <|> Function <$ symbol "function" <|> Method <$ symbol "method"
  retType <- lexeme $ Void <$ symbol "void" <|> Type <$> type'
  name <- lexeme identifier
  param <- lexeme $ parens parameterList
  SubroutineDec decType retType name param <$> body
  where
    body = braces $ SubroutineBody <$> many varDec <*> many stmt
    param = Param <$> type' <*> identifier
    parameterList = SubroutineParam <$> (liftA2 (:) param (many (symbol "," *> param)) <|> pure [])

type' :: Parser Type
type' =
  choice
    [ IntType <$ symbol "int",
      CharType <$ symbol "char",
      BoolType <$ symbol "boolean",
      ClassName <$> identifier
    ]

classVarDec :: Parser ClassVarDec
classVarDec =
  lexeme $
    ClassVarDec <$> (Field <$ symbol "field" <|> Static <$ symbol "static") <*> type' <*> varNames <* symbol ";"
  where
    varNames = (:) <$> identifier <*> many (symbol "," *> identifier)

parseClass :: Parser Class
parseClass = apply2 $ sc *> (Class <$ symbol "class" <*> identifier)
  where
    apply2 f = uncurry <$> f <*> braces ((,) <$> many classVarDec <*> many subroutineDec)

parseProgram :: String -> String -> Either (ParseErrorBundle String Void) Class
parseProgram = parse parseClass