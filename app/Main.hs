module Main where

import Token (tokenize)
import Expr (shuntingYard)
import Control.Monad.Trans.Maybe ( MaybeT(runMaybeT) )
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
    liftIO $ print tokens
    let rpn = shuntingYard tokens []
    liftIO $ print rpn
  loop