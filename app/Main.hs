module Main where

import Calc (calc, Calculator, runCalculator, liftIO)

main :: IO ()
main = do
  putStrLn "Rechner"
  runCalculator loop



loop :: Calculator () ()
loop = do
  line <- liftIO getLine
  result <- calc line
  case result of
    Left err -> liftIO (print err) >> loop
    Right d -> liftIO (print d) >> loop