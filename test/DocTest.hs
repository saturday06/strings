-- {-# OPTIONS_GHC -F -pgmF doctest-discover -optF test/DocTest.json #-}

module Main where

import           System.Directory
import           System.FilePath
import           Test.DocTest

sources :: FilePath -> IO [FilePath]
sources path = do
  pathIsDir <- doesDirectoryExist path
  if pathIsDir then do
    children <- fmap (filter (\child -> "." /= take 1 child)) $ listDirectory path
    fmap (foldl (++) []) $ mapM sources $ map (path </>) children
  else do
    return [path]

main :: IO ()
main = doctest =<< sources "src"
