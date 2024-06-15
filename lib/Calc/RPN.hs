{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}
module Calc.RPN where

import Control.Monad.Trans.Except
import Control.Monad.IO.Class (MonadIO(liftIO))

import Calc.Function (Fun, evaluateFunction)
import Calc.Value (Value)
import Calc.Error (Error(..), ErrorMessage(..), Position)


data RPN =
  RPN'Value Position Value |
  forall f. Fun f => RPN'Function Position String f
instance Show RPN where
  show (RPN'Value _ v) = show v
  show (RPN'Function _ name _) = name


evaluate :: [RPN] -> Except Error Value
evaluate = go []
  where
    go :: [(Position, Value)] -> [RPN] -> Except Error Value
    go []       [] = throwE $ Error mempty $ Message "empty input"
    go [(_, v)] [] = return v
    go (_ : _) [] = throwE $ Error mempty $ Message ""
    go ds (RPN'Function pos _ f : rest) = do
      ds' <- evaluateFunction pos ds f
      go ds' rest
    go ds (RPN'Value pos v : rest)  = go ((pos, v) : ds) rest