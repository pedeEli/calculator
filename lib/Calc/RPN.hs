{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}
module Calc.RPN where

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.Ratio (Ratio, numerator, denominator)

import Calc.Unit (multiply, divide)
import Calc.Function (Fun, evaluateFunction)
import Calc.Value (Value, isStandaloneUnit)


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
    go (v1 : v2 : rest) []
      | isStandaloneUnit v1 || isStandaloneUnit v2 = go (v1 * v2 : rest) []
      | otherwise = liftIO (putStrLn "missing function") >> fail ""
    -- go (_ : _)  [] = liftIO (putStrLn "missing function") >> fail ""
    go ds (RPN'Function _ f : rest) = go (evaluateFunction ds f) rest
    go ds (RPN'Value v : rest)  = go (v : ds) rest