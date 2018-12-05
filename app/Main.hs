module Main where

import Lexer
import Parser
import Text.PrettyPrint
import ParserCombinator
import System.Environment

main :: IO ()
main = do
  [f] <- getArgs
  code <- readFile f
  putStrLn . show . parse prog $ code 
