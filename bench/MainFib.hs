module Main where

import Datatypes
import Fib
import System.Environment

main :: IO ()
main = do
  arg <- head <$> getArgs
  let test = read arg :: Int
  putStrLn $ "Fibonacci"
  putStrLn . show . fib $ test
