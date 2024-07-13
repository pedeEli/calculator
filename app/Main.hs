module Main where

import Control.Monad.IO.Class

import GCI.Calculator

import GCI.Types.Value as V
import System.IO


main :: IO ()
main = do
  putStrLn "Rechner"
  startCalculator $ do
    
    addBuildIn2 "+" 0 (<<+>>)
    addBuildIn2 "-" 0 (<<->>)
    addBuildIn2 "*" 1 (<<*>>)
    addBuildIn2 "/" 1 (<</>>)
    addBuildIn2 "^" 2 (<<^>>)

    addBuildIn1 "negate" V.negate
    
    loop


loop :: Calculator ()
loop = do
  liftIO $ putStr "> " >> hFlush stdout
  str <- liftIO getLine
  result <- interpret str
  case result of
    Success -> loop
    ValI val -> do
      liftIO $ putStrLn $ "= " ++ val
      loop
    ErrorI lerr -> do
      liftIO $ print lerr
      loop