{-# LANGUAGE TemplateHaskell, ExistentialQuantification, FlexibleContexts #-}
module Calc.ShuntingYard where

import Data.List (find)
import Data.Maybe (isJust)

import Control.Lens
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.IO.Class (MonadIO(liftIO))

import Calc.Token (Token(..))
import Calc.RPN (RPN(..))
import Calc.Value(Value)
import Calc.Function (Fun)

data OperatorInfo = OperatorInfo {
  _opType :: String,
  _opPrecedence :: Word,
  _fun :: Value -> Value -> Value}

$(makeLenses 'OperatorInfo)

buildInOperators :: [OperatorInfo]
buildInOperators = [
  OperatorInfo "+" 0 (flip (+)),
  OperatorInfo "-" 0 (flip (-)),
  OperatorInfo "*" 1 (flip (*)),
  OperatorInfo "/" 1 (flip (/))]

data OperatorStackType =
  OperatorStackType'Operator OperatorInfo |
  forall f. Fun f => OperatorStackType'Function String f |
  OperatorStackType'Bracket Char Char -- closing and opening bracket

$(makePrisms ''OperatorStackType)

type ShuntingYard = StateT [OperatorStackType] (MaybeT IO) [RPN]

shuntingYard' :: Bool -> [Token] -> ShuntingYard
shuntingYard' _ [] = get >>= go
  where
    go :: [OperatorStackType] -> ShuntingYard
    go [] = return []
    go (OperatorStackType'Bracket closing _ : _) = liftIO (putStrLn $ "missing bracket " ++ [closing]) >> lift (fail "")
    go (OperatorStackType'Operator info : rest) = go rest <&> (RPN'Function (_opType info) (_fun info) :)
    go (OperatorStackType'Function name f : rest) = go rest <&> (RPN'Function name f :)
shuntingYard' start (t : ts) = case t of
  Token'Value value _ -> shuntingYard' False ts <&> (RPN'Value value :)
  Token'OpeningBracket closing opening -> modify (OperatorStackType'Bracket closing opening :) >> shuntingYard' False ts
  Token'ClosingBracket closing -> (++) <$> popUntilBracket closing <*> shuntingYard' False ts
  Token'Operator operator -> case find (\a -> operator == _opType a) buildInOperators of
    Nothing -> liftIO (putStrLn $ "unknown operator " ++ operator) >> lift (fail "")
    Just info -> do
      stack <- get
      let isMinus = _opType info == "-"
          stackHead = stack ^? _head . _OperatorStackType'Bracket
      if isMinus && (start || isJust stackHead)
        then modify (OperatorStackType'Function "negate" (negate :: Value -> Value) :) >> shuntingYard' False ts
        else (++) <$> popOperators info <*> shuntingYard' False ts

popOperators :: OperatorInfo -> ShuntingYard
popOperators info = get >>= go
  where
    go :: [OperatorStackType] -> ShuntingYard
    go [] = put [OperatorStackType'Operator info] >> return []
    go stack@(OperatorStackType'Bracket _ _ : _) = put (OperatorStackType'Operator info : stack) >> return []
    go       (OperatorStackType'Function name f : rest) = go rest <&> (RPN'Function name f :)
    go stack@(OperatorStackType'Operator i : rest) = if _opPrecedence i < _opPrecedence info
      then put (OperatorStackType'Operator info : stack) >> return []
      else go rest <&> (RPN'Function (_opType i) (_fun i) :)


popUntilBracket :: Char -> ShuntingYard
popUntilBracket bracket = get >>= go []
  where
    go :: [RPN] -> [OperatorStackType] -> ShuntingYard
    go _ [] = liftIO (putStrLn "missing bracket") >> lift (fail "")
    go output (OperatorStackType'Operator info : rest) = go (RPN'Function (_opType info) (_fun info) : output) rest
    go output (OperatorStackType'Function name f : rest) = go (RPN'Function name f : output) rest
    go output (OperatorStackType'Bracket closing _ : rest) = if bracket == closing
      then put rest >> return output
      else liftIO (putStrLn $ "missmatched brackets " ++ [bracket] ++ " and " ++ [closing]) >> lift (fail "")



shuntingYard :: [Token] -> MaybeT IO [RPN]
shuntingYard tokens = evalStateT (shuntingYard' True tokens) []
