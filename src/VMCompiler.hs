{-# LANGUAGE OverloadedStrings #-}

module VMCompiler where

import Control.Applicative
    ( Applicative(pure), (<$>), Alternative((<|>)) )
import Control.Monad.Except
    ( Monad(return),
      MonadFail(fail),
      mapM,
      mapM_,
      forM_,
      ExceptT,
      MonadError(throwError),
      runExceptT )
import Control.Monad.State
    ( Monad(return),
      MonadFail(fail),
      mapM,
      mapM_,
      forM_,
      gets,
      evalState,
      MonadState(put, get),
      State )
import Data.List (dropWhileEnd, isSuffixOf)
import qualified Data.Map as M
import Data.Maybe ( Maybe(..), maybe, fromMaybe )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy.Builder (fromText)
import Parser ( parseProgram )
import System.IO ( IO, readFile, writeFile, FilePath )
import Text.Megaparsec (errorBundlePretty)
import Types
import Prelude hiding (lookup)
import Utils ( baseName, listFiles )

data VarKind
  = FieldKind
  | StaticKind
  | ArgumentKind
  | LocalKind
  | PointerKind
  deriving (Eq)

instance Show VarKind where
  show FieldKind = "this"
  show StaticKind = "static"
  show ArgumentKind = "argument"
  show LocalKind = "local"
  show PointerKind = "pointer"

data SymbolTableInfo = SymbolTableInfo
  { varType :: Type,
    kind :: VarKind,
    index :: Int
  }
  deriving (Show)

data CodegenInfo = CodegenInfo
  { parentClass :: String,
    symbolTables :: [SymbolTable],
    labelId :: Int
  }
  deriving (Show)

type SymbolTable = M.Map Name SymbolTableInfo

type Env = State CodegenInfo

type CompilationEnv = ExceptT String Env

unlinesConcise :: [String] -> String
unlinesConcise = T.unpack . T.strip . T.pack . unlines . filter (not . null)

runCompileEnv :: CompilationEnv String -> CodegenInfo -> Either String String
runCompileEnv env = evalState (runExceptT env)

countVar :: VarKind -> SymbolTable -> Int
countVar k = M.size . M.filter ((== k) . kind)

insertAll :: Ord k => M.Map k SymbolTableInfo -> [k] -> Type -> VarKind -> Int -> M.Map k SymbolTableInfo
insertAll m vars type' kind startId =
  foldl (\m (id, var) -> M.insert var SymbolTableInfo {varType = type', kind = kind, index = id} m) m (zip [startId ..] vars)

classVarDec :: ClassVarDec -> CompilationEnv ()
classVarDec (ClassVarDec decType type' vars) = do
  info <- get
  classTable <- gets (head . symbolTables)
  let kind = getKind decType
  -- Symbol table list is always empty at the start of each class
  put $ info {symbolTables = [insertAll classTable vars type' kind (countVar kind classTable)]}
  where
    getKind Field = FieldKind
    getKind Static = StaticKind

subroutineVarDec :: SubroutineVarDec -> CompilationEnv ()
subroutineVarDec (SubroutineVarDec type' vars) = do
  st <- get
  -- New symbol table must be created before enter each subroutine
  -- Assume that the first one is the correct one
  let (m : ms) = symbolTables st
  put st {symbolTables = insertAll m vars type' LocalKind (countVar LocalKind m) : ms}

lookup :: Name -> [SymbolTable] -> Maybe SymbolTableInfo
lookup var [] = Nothing
lookup var (m : ms) = M.lookup var m <|> lookup var ms

pushConst :: Show a => a -> String
pushConst n = "push constant " <> show n

compileExpr :: Expr -> CompilationEnv String
compileExpr (Int x) = return $ pushConst x
compileExpr (String s) =
  let appendChar c = pushConst (fromEnum c)
   in return $
        unlinesConcise
          [ pushConst (length s),
            "call String.new 1",
            -- Stack currently contains base address of the new object
            -- Append all chars
            unlinesConcise $ map (\c -> unlinesConcise [appendChar c, "call String.appendChar 2"]) s
          ]
compileExpr (Unary op expr) = do
  compiledExpr <- compileExpr expr
  return $ compiledExpr <> "\n" <> (if op == UnaryDash then "neg" else "not")
compileExpr (Binary op a b) = do
  lhs <- compileExpr a
  rhs <- compileExpr b
  return $ unlinesConcise [lhs, rhs, toStr op]
  where
    toStr Plus = "add"
    toStr Minus = "sub"
    toStr Ampersand = "and"
    toStr Pipe = "or"
    toStr Star = "call Math.multiply 2"
    toStr Slash = "call Math.divide 2"
    toStr LessThan = "lt"
    toStr GreaterThan = "gt"
    toStr Equal = "eq"
compileExpr (Call target fname params) = do
  -- Since Jack compiler is one-pass, it assumes that if target is not present in symbol tables,
  -- it must be class name or function name
  tables <- gets symbolTables
  params <- mapM compileExpr params
  pClass <- gets parentClass
  let name = fromMaybe "this" target
  return $
    maybe
      ( unlinesConcise
          [ unlinesConcise params,
            "call " <> name <> "." <> fname <> " " <> show (length params)
          ]
      )
      ( \info ->
          unlinesConcise
            [ "push " <> show (kind info) <> " " <> show (index info), -- pass this
              unlinesConcise params,
              unwords ["call", className (varType info) <> "." <> fname, show (length params + 1)]
            ]
      )
      (lookup name tables)
  where
    className (ClassName name) = name
compileExpr (Keyword kw) = return $ case kw of
  KWTrue -> "push constant 1\nneg"
  KWThis -> "push pointer 0"
  _ -> "push constant 0"
compileExpr (Var name expr) = do
  tables <- gets symbolTables
  case lookup name tables of
    Nothing -> throwError ""
    Just info -> do
      compiledExpr <- maybe (pure "") compileExpr expr
      if null compiledExpr
        then return $ "push " <> show (kind info) <> " " <> show (index info)
        else
          return $ -- array access
            unlinesConcise
              [ "push " <> show (kind info) <> " " <> show (index info),
                compiledExpr,
                "add",
                "pop pointer 1",
                "push that 0"
              ]

compileStmt :: Stmt -> CompilationEnv String
compileStmt (Let var arrExpr rhs) = do
  tables <- gets symbolTables
  case lookup var tables of
    Nothing -> throwError $ "Used of undeclared variable '" <> var <> "'"
    Just info -> do
      compiledRhs <- compileExpr rhs
      case arrExpr of
        Nothing -> return $ compiledRhs <> "\n" <> unwords ["pop", show (kind info), show $ index info]
        Just expr -> do
          compiledArrExpr <- compileExpr expr
          return $
            unlinesConcise
              [ "push " <> show (kind info) <> " " <> show (index info),
                compiledArrExpr,
                "add",
                compiledRhs,
                "pop temp 0",
                "pop pointer 1",
                "push temp 0",
                "pop that 0"
              ]
compileStmt (If cond trueBlock elseBlock) = do
  compiledCond <- compileExpr cond
  compiledTrueBlock <- mapM compileStmt trueBlock
  compiledElseBlock <- mapM compileStmt (fromMaybe [] elseBlock)

  st <- get
  id <- gets labelId
  put st {labelId = id + 1}

  return $
    unlinesConcise
      [ compiledCond,
        "not",
        "if-goto IF_FALSE_" <> show id,
        unlinesConcise compiledTrueBlock,
        "goto IF_END_" <> show id,
        "label IF_FALSE_" <> show id,
        unlinesConcise compiledElseBlock,
        "label IF_END_" <> show id
      ]
compileStmt (While cond block) = do
  compiledCond <- compileExpr cond
  compiledBlock <- mapM compileStmt block

  st <- get
  id <- gets labelId
  put st {labelId = id + 1}

  return $
    unlinesConcise
      [ "label WHILE_" <> show id,
        compiledCond,
        "not",
        "if-goto WHILE_END_" <> show id,
        unlinesConcise compiledBlock,
        "goto WHILE_" <> show id,
        "label WHILE_END_" <> show id
      ]
compileStmt (Do expr) = do
  compiledExpr <- compileExpr expr
  return $
    unlinesConcise
      [ compiledExpr,
        "pop temp 0" -- dump returned value
      ]
compileStmt (Return expr) =
  case expr of
    Just e -> do
      compiledExpr <- compileExpr e
      return $
        unlinesConcise
          [ compiledExpr,
            "return"
          ]
    _ -> return ""

compileSubroutineDec :: SubroutineDec -> CompilationEnv String
compileSubroutineDec (SubroutineDec decType ret fname (SubroutineParam params) (SubroutineBody vars stmts)) = do
  info <- get
  -- Grab the last one as the class table
  classTable <- gets (last . symbolTables)
  className <- gets parentClass
  let fieldCount = M.size $ M.filter ((== FieldKind) . kind) classTable
  let argMap =
        foldl
          (\m (id, param) -> M.insert (name param) SymbolTableInfo {varType = type' param, kind = ArgumentKind, index = id} m)
          M.empty
          (zip [0 ..] ([Param (ClassName className) "this" | decType == Method] <> params))

  -- Only maintain two map
  put $ info {symbolTables = argMap : [classTable]}

  mapM_ subroutineVarDec vars
  compiledBody <- mapM compileStmt stmts

  localTable <- gets (head . symbolTables)
  let func = "function " <> parentClass info <> "." <> fname <> " " <> show (countVar LocalKind localTable)

  case decType of
    Constructor -> do
      return $
        unlinesConcise
          [ func,
            pushConst (countVar FieldKind classTable),
            "call Memory.alloc 1",
            "pop pointer 0",
            unlinesConcise compiledBody
          ]
    _ ->
      return $
        unlinesConcise
          [ func, -- Push 'this'
            if decType == Method then "push argument 0\npop pointer 0" else "",
            unlinesConcise compiledBody,
            if ret == Void then pushConst 0 <> "\nreturn" else ""
          ]
  where
    name (Param _ n) = n
    type' (Param t _) = t

compileClass :: Class -> CompilationEnv String
compileClass (Class name vars subs) = do
  info <- get
  let this = SymbolTableInfo {varType = ClassName name, kind = PointerKind, index = 0}
  put info {symbolTables = [M.insert "this" this M.empty], parentClass = name}

  forM_ vars classVarDec
  compiledSubs <- mapM compileSubroutineDec subs

  return $ unlinesConcise compiledSubs

compile :: String -> IO ()
compile fname = do
  contents <- readFile fname
  case parseProgram fname contents of
    Right class' -> do
      let result = runCompileEnv (compileClass class') CodegenInfo {symbolTables = [], labelId = 0, parentClass = ""}
      either fail (writeFile $ baseName fname <> ".vm") result
    Left e -> fail (errorBundlePretty e)

compileAllJack :: FilePath -> IO ()
compileAllJack path = do
  jackFiles <- filter (".jack" `isSuffixOf`) <$> listFiles path
  forM_ jackFiles compile