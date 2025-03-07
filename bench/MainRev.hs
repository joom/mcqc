module Main where

import Datatypes
import Rev
import System.Environment

deriving instance (Show a) => Show (Coq_list a)

main :: IO ()
main = do
  arg <- head <$> getArgs
  let test = series (read arg :: Int)
  putStrLn $ "Reversed"
  putStrLn . show . rev $ test
