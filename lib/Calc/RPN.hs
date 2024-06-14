{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}
module Calc.RPN where

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Calc.Function (Fun, evaluateFunction)
import Calc.Value (Value)


data RPN =
  RPN'Value Value |
  forall f. Fun f => RPN'Function String f
instance Show RPN where
  show (RPN'Value v) = show v
  show (RPN'Function name _) = name


evaluate :: [RPN] -> MaybeT IO Value
evaluate = go []
  where
    go :: [Value] -> [RPN] -> MaybeT IO Value
    go []       [] = error "not possible"
    go [v] [] = return v
    go (_ : _) [] = liftIO (putStrLn "missing function") >> fail ""
    go ds (RPN'Function _ f : rest) = go (evaluateFunction ds f) rest
    go ds (RPN'Value v : rest)  = go (v : ds) rest