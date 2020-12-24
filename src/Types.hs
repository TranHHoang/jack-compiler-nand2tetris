module Types
  ( Expr (..),
    Stmt (..),
    Class (..),
    KeywordConst (..),
    UnaryOp (..),
    BinaryOp (..),
    Type (..),
    ClassVarDecType (..),
    ClassVarDec (..),
    SubroutineDec (..),
    SubroutineVarDec (..),
    SubroutineDecType (..),
    SubroutineDecReturnType (..),
    SubroutineBody (..),
    SubroutineParam (..),
    Param (..),
    Name,
    keywordConsts,
    binaryOps,
  )
where

import Data.Text (Text)

type Name = String

data KeywordConst
  = KWTrue
  | KWFalse
  | KWNull
  | KWThis
  deriving (Show, Eq)

data UnaryOp
  = UnaryDash
  | UnaryTilde
  deriving (Show, Eq)

data BinaryOp
  = Plus
  | Minus
  | Star
  | Slash
  | Ampersand
  | Pipe
  | LessThan
  | GreaterThan
  | Equal
  deriving (Show, Eq)

data Type
  = IntType
  | CharType
  | BoolType
  | ClassName Name
  deriving (Show, Eq)

data Expr
  = Int Integer
  | String String
  | Keyword KeywordConst
  | Var Name (Maybe Expr)
  -- | ArrayAccess Name Expr
  | Call (Maybe Name) Name [Expr]
  | Unary UnaryOp Expr
  | Binary BinaryOp Expr Expr
  deriving (Show, Eq)

data Stmt
  = Let Name (Maybe Expr) Expr -- 'let' varName ('[' expr ']')? '=' expr ';'
  | If Expr [Stmt] (Maybe [Stmt])
  | While Expr [Stmt]
  | Do Expr
  | Return (Maybe Expr)
  deriving (Show, Eq)

data ClassVarDecType
  = Field
  | Static
  deriving (Show, Eq)

data ClassVarDec = ClassVarDec ClassVarDecType Type [Name] deriving (Show, Eq)

data SubroutineDecType
  = Constructor
  | Function
  | Method
  deriving (Show, Eq)

data SubroutineDecReturnType
  = Void
  | Type Type
  deriving (Show, Eq)

data Param
  = Param Type Name
  deriving (Show)

newtype SubroutineParam
  = SubroutineParam [Param]
  deriving (Show)

data SubroutineDec
  = SubroutineDec SubroutineDecType SubroutineDecReturnType Name SubroutineParam SubroutineBody
  deriving (Show)

data SubroutineVarDec
  = SubroutineVarDec Type [Name]
  deriving (Show)

data SubroutineBody
  = SubroutineBody [SubroutineVarDec] [Stmt]
  deriving (Show)

data Class
  = Class Name [ClassVarDec] [SubroutineDec]
  deriving (Show)

keywordConsts :: [([Char], KeywordConst)]
keywordConsts = [("this", KWThis), ("null", KWNull), ("true", KWTrue), ("false", KWFalse)]

binaryOps :: [(Char, BinaryOp)]
binaryOps =
  [ ('+', Plus),
    ('-', Minus),
    ('*', Star),
    ('/', Slash),
    ('&', Ampersand),
    ('|', Pipe),
    ('<', LessThan),
    ('>', GreaterThan),
    ('=', Equal)
  ]