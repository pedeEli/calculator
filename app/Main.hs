module Main where

import Control.Monad.IO.Class

import GCI.Calculator

import GCI.Types.Value as V
import GCI.Types.SrcLoc

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
      printError str lerr
      loop


printError :: String -> Located String -> Calculator ()
printError str (L loc err) = do
  liftIO $ putStrLn "error:"
  if loc == mempty
    then liftIO $ putStrLn $ "    \ESC[31m" ++ str ++ "\n\t^ " ++ err ++ "\ESC[37m" 
    else do
      let s = srcSpanS loc - 1
          e = srcSpanE loc - 1
          (str1, str') = splitAt s str
          (str2, str3) = splitAt (e - s) str'
      liftIO $ putStrLn $ "    " ++ str1 ++ "\ESC[31m" ++ str2 ++ "\ESC[37m" ++ str3
      liftIO $ putStrLn $ "    \ESC[31m" ++ replicate s ' ' ++ "^ " ++ err ++ "\ESC[37m"