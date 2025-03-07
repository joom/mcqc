module Main where

import Datatypes
import Sort
import System.Environment

deriving instance (Show a) => Show (Coq_list a)

deriving instance (Show a) => Show (Coq_option a)

main :: IO ()
main = do
  arg <- head <$> getArgs
  let test = series (read arg :: Int)
  putStrLn $ "Sorted"
  putStrLn . show . sort $ test
