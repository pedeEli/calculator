module Main where

import GCI.Calculator
import Control.Monad.IO.Class

main :: IO ()
main = do
  putStrLn "Rechner"
  startCalculator loop


loop :: Calculator ()
loop = do
  str <- liftIO getLine
  result <- interpret str
  case result of
    Nothing -> loop
    Just s -> do
      liftIO $ putStrLn s
      loop