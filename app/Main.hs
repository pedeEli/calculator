module Main where

import Token (tokenize)
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
    result <- tokenize (line ++ "\n")
    liftIO $ print result
  loop
      






data Expr = Mult Expr Expr
          | Add Expr Expr
          | Val Double