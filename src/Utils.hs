{-# LANGUAGE OverloadedStrings #-}
module Utils (baseName, listFiles, combinePath, parentDir) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List ( dropWhileEnd, isSuffixOf )
import Control.Monad ( foldM )
import System.Directory ( doesDirectoryExist, listDirectory )

baseName :: FilePath -> String
baseName path = dropExt . T.unpack . go $ T.splitOn "/" (T.reverse . T.pack $ path)
  where
    dropExt s = let name = dropWhileEnd (/= '.') s in if name == "" then s else init name
    go [] = ""
    go ("" : xs) = go xs
    go (x : _) = T.reverse x

listFiles :: FilePath -> IO [FilePath]
listFiles root = go root []
  where
    go :: FilePath -> [FilePath] -> IO [FilePath]
    go path ls = do
      entries <- map ((path <> "/") <>) <$> listDirectory path
      (files, dirs) <- partitionM doesDirectoryExist entries
      foldM (\ls dir -> (ls <>) <$> go dir ls) files dirs

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM pre = go pre [] []
  where
    go _ a b [] = pure (a, b)
 
combinePath :: FilePath -> String -> String
combinePath path name = if "/" `isSuffixOf` path then path <> name else path <> "/" <> name

parentDir :: FilePath -> String
parentDir path = T.unpack $ foldl1 (\acc x -> acc <> "/" <> x) (init (T.splitOn "/" (T.pack path)))