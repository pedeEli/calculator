module Main where

import Text.Parsec
import Token

main :: IO ()
main = do
  putStrLn "Rechner"
  loop



loop :: IO ()
loop = do
  line <- getLine
  result <- runParserT tokenizer () "" line
  case result of
    Left err -> print err >> loop
    Right tokens -> print tokens >> loop






data Expr = Mult Expr Expr
          | Add Expr Expr
          | Val Double