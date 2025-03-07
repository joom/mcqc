module Main where

import Datatypes
import Fact
import System.Environment

main :: IO ()
main = do
  arg <- head <$> getArgs
  let test = read arg :: Int
  putStrLn $ "Factorial"
  putStrLn . show . fact $ test
