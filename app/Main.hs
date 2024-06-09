module Main where

import Calc (calc)

main :: IO ()
main = do
  putStrLn "Rechner"
  loop



loop :: IO ()
loop = do
  line <- getLine
  result <- calc (line ++ "\n")
  case result of
    Nothing -> loop
    Just d -> putStrLn d >> loop