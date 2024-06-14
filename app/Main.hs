module Main where

import Calc (calc)

main :: IO ()
main = do
  putStrLn "Rechner"
  loop



loop :: IO ()
loop = do
  line <- getLine
  let result = calc (line ++ "\n")
  case result of
    Left err -> print err >> loop
    Right d -> print d >> loop