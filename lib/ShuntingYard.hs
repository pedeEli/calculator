module ShuntingYard (shuntingYard) where

import Token (Token(..))
import RPN (RPN(..), OperatorInfo(..), buildInOperators)

import Data.List (singleton, find)
import Data.Functor ((<&>))

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.IO.Class (MonadIO(liftIO))


shuntingYard :: [Token] -> MaybeT IO [RPN]
shuntingYard tokens = evalStateT (shuntingYard' tokens) []

data OperatorStackType =
  OperatorStackType'Operator OperatorInfo |
  OperatorStackType'Bracket Char Char -- closing and opening bracket

type ShuntingYard = StateT [OperatorStackType] (MaybeT IO) [RPN]

shuntingYard' :: [Token] -> ShuntingYard
shuntingYard' [] = get >>= go
  where
    go :: [OperatorStackType] -> ShuntingYard
    go [] = return []
    go (OperatorStackType'Bracket closing _ : _) = liftIO (putStrLn $ "missing bracket " ++ [closing]) >> lift (fail "")
    go (OperatorStackType'Operator info : rest) = go rest <&> (RPN'Operator info :)
shuntingYard' (t : ts) = case t of
  Token'Value value unit -> shuntingYard' ts <&> (RPN'Value value unit :)
  Token'OpeningBracket closing opening -> modify (OperatorStackType'Bracket closing opening :) >> shuntingYard' ts
  Token'ClosingBracket closing -> (++) <$> popUntilBracket closing <*> shuntingYard' ts
  Token'Operator operator -> case find (\a -> operator == opType a) buildInOperators of
    Nothing -> liftIO (putStrLn $ "unknown operator " ++ operator) >> lift (fail "")
    Just info -> (++) <$> popOperators info <*> shuntingYard' ts


popOperators :: OperatorInfo -> ShuntingYard
popOperators info = get >>= go
  where
    go :: [OperatorStackType] -> ShuntingYard
    go [] = put [OperatorStackType'Operator info] >> return []
    go stack@(OperatorStackType'Bracket _ _ : _) = put (OperatorStackType'Operator info : stack) >> return []
    go stack@(OperatorStackType'Operator i : rest) = if opPrecedence i < opPrecedence info
      then put (OperatorStackType'Operator info : stack) >> return []
      else popOperators info


popUntilBracket :: Char -> ShuntingYard
popUntilBracket bracket = get >>= go []
  where
    go :: [RPN] -> [OperatorStackType] -> ShuntingYard
    go _ [] = liftIO (putStrLn "missing bracket") >> lift (fail "")
    go output (OperatorStackType'Operator info : rest) = go (RPN'Operator info : output) rest
    go output (OperatorStackType'Bracket closing _ : rest) = if bracket == closing
      then put rest >> return output
      else liftIO (putStrLn $ "missmatched brackets " ++ [bracket] ++ " and " ++ [closing]) >> lift (fail "")