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
    Success -> loop
    ValI val -> do
      liftIO $ putStrLn val
      loop
    ErrorI lerr -> do
      liftIO $ print lerr
      loop