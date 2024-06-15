{-# LANGUAGE TemplateHaskell, ExistentialQuantification, FlexibleContexts #-}
module Calc.ShuntingYard where

import Data.List (find)
import Data.Maybe (isJust)

import Control.Lens
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.IO.Class (MonadIO(liftIO))

import Calc.Token (Token(..), TokenType(..))
import Calc.RPN (RPN(..))
import Calc.Value
import Calc.Function (Fun)
import Calc.Error (Error(..), Position, ErrorMessage(..))

data OperatorInfo = OperatorInfo {
  _opType :: String,
  _opPrecedence :: Word,
  _fun :: Value -> Value -> Except String Value}

$(makeLenses 'OperatorInfo)

buildInOperators :: [OperatorInfo]
buildInOperators = [
  OperatorInfo "+" 0 (flip (<<+>>)),
  OperatorInfo "-" 0 (flip (<<->>)),
  OperatorInfo "*" 1 (flip (<<*>>)),
  OperatorInfo "/" 1 (flip (<</>>))]

data OperatorStackType =
  OperatorStackType'Operator Position OperatorInfo |
  forall f. Fun f => OperatorStackType'Function Position String f |
  OperatorStackType'Bracket Position Char Char -- closing and opening bracket

$(makePrisms ''OperatorStackType)

type ShuntingYard = StateT [OperatorStackType] (Except Error) [RPN]

shuntingYard' :: Bool -> [Token] -> ShuntingYard
shuntingYard' _ [] = get >>= go
  where
    go :: [OperatorStackType] -> ShuntingYard
    go [] = return []
    go (OperatorStackType'Bracket pos _ _ : _) = lift $ throwE $ Error pos $ Message "missing closing bracket"
    go (OperatorStackType'Operator pos info : rest) = go rest <&> (RPN'Function pos (_opType info) (_fun info) :)
    go (OperatorStackType'Function pos name f : rest) = go rest <&> (RPN'Function pos name f :)
shuntingYard' start (t : ts) = case t of
  Token (Token'Value value) pos -> shuntingYard' False ts <&> (RPN'Value pos value :)
  Token (Token'OpeningBracket closing opening) pos -> modify (OperatorStackType'Bracket pos closing opening :) >> shuntingYard' False ts
  Token (Token'ClosingBracket closing) pos -> (++) <$> popUntilBracket pos closing <*> shuntingYard' False ts
  Token (Token'Operator operator) pos -> case find (\a -> operator == _opType a) buildInOperators of
    Nothing -> lift $ throwE $ Error pos $ Message "unknown operator"
    Just info -> do
      stack <- get
      let isMinus = _opType info == "-"
          stackHead = stack ^? _head . _OperatorStackType'Bracket
      if isMinus && (start || isJust stackHead)
        then modify (OperatorStackType'Function pos "negate" vNegate :) >> shuntingYard' False ts
        else (++) <$> popOperators pos info <*> shuntingYard' False ts

popOperators :: Position -> OperatorInfo -> ShuntingYard
popOperators pos info = get >>= go
  where
    go :: [OperatorStackType] -> ShuntingYard
    go [] = put [OperatorStackType'Operator pos info] >> return []
    go stack@(OperatorStackType'Bracket {} : _) = put (OperatorStackType'Operator pos info : stack) >> return []
    go       (OperatorStackType'Function pos name f : rest) = go rest <&> (RPN'Function pos name f :)
    go stack@(OperatorStackType'Operator pos i : rest) = if _opPrecedence i < _opPrecedence info
      then put (OperatorStackType'Operator pos info : stack) >> return []
      else go rest <&> (RPN'Function pos (_opType i) (_fun i) :)


popUntilBracket :: Position -> Char -> ShuntingYard
popUntilBracket pos bracket = get >>= go []
  where
    go :: [RPN] -> [OperatorStackType] -> ShuntingYard
    go _ [] = lift $ throwE $ Error pos $ Message "missing opening bracket"
    go output (OperatorStackType'Operator pos info : rest) = go (RPN'Function pos (_opType info) (_fun info) : output) rest
    go output (OperatorStackType'Function pos name f : rest) = go (RPN'Function pos name f : output) rest
    go output (OperatorStackType'Bracket pos closing _ : rest) = if bracket == closing
      then put rest >> return output
      else lift $ throwE $ Error pos $ Message "missmatched brackets"



shuntingYard :: [Token] -> Except Error [RPN]
shuntingYard tokens = evalStateT (shuntingYard' True tokens) []
