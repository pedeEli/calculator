{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}
module Calc.RPN where


import qualified Calc.Calculator as C
import qualified Calc.Error as E
import qualified Calc.Value as V


data RPN =
  Ast E.Position (C.Ast V.Value) |
  forall f. C.Fun f => BuildIn E.Position String f
instance Show RPN where
  show (Ast _ v) = show v
  show (BuildIn _ name _) = name



getPosition :: RPN -> E.Position
getPosition (Ast    pos _)   = pos
getPosition (BuildIn pos _ _) = pos


rpnToPosition :: [RPN] -> E.Position
rpnToPosition [] = mempty
rpnToPosition rpn = getPosition (head rpn) <> getPosition (last rpn)


evaluate :: [RPN] -> C.Calculator () V.Value
evaluate = go []
  where
    go :: [(E.Position, V.Value)] -> [RPN] -> C.Calculator () V.Value
    go []       [] = C.throwString mempty "empty input"
    go [(_, v)] [] = return v
    go (_ : _) [] = C.throwString mempty "syntax error"
    go ds (BuildIn pos _ f : rest) = do
      ds' <- C.evaluateFunction pos ds f
      go ds' rest
    go ds (Ast pos v : rest)  = go ((pos, v) : ds) rest