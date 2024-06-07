module Main where

import Token (tokenize)
import ShuntingYard (shuntingYard)
import RPN (evaluate)
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Control.Monad.IO.Class (MonadIO(liftIO))

main :: IO ()
main = do
  putStrLn "Rechner"
  loop



loop :: IO ()
loop = do
  runMaybeT $ do
    line <- liftIO getLine
    tokens <- tokenize (line ++ "\n")
    rpn <- shuntingYard tokens
    liftIO $ putStrLn $ evaluate [] rpn
  loop