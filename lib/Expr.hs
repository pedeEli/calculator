module Expr (shuntingYard) where

import Token (Token(Value, OpeningBracket, ClosingBracket))
import qualified Token (Token(Operator))

import Data.List (singleton, find)
import Data.Functor ((<&>))

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.IO.Class (MonadIO(liftIO))

data Expr =
  Mult Expr Expr |
  Add Expr Expr |
  Power Expr Expr |
  Val Double


data OperatorInfo = OperatorInfo {opType :: String, opPrecedence :: Word}
  deriving (Show)

buildInOperators :: [OperatorInfo]
buildInOperators = [
  OperatorInfo "+" 0,
  OperatorInfo "*" 1,
  OperatorInfo "^" 2]


data OperatorStack = Operator OperatorInfo | Bracket Char Char
  deriving (Show)

shuntingYard :: [Token] -> MaybeT IO [String]
shuntingYard tokens = evalStateT (shuntingYard' tokens) []

type ShuntingYard = StateT [OperatorStack] (MaybeT IO)

shuntingYard' :: [Token] -> ShuntingYard [String]
shuntingYard' [] = get >>= go
  where
    go :: [OperatorStack] -> ShuntingYard [String]
    go [] = return []
    go (Bracket closing _ : _) = liftIO (putStrLn $ "missing bracket " ++ [closing]) >> lift (fail "")
    go (Operator info : rest) = go rest <&> (opType info :)
shuntingYard' (t : ts) = case t of
  Value v -> shuntingYard' ts <&> (show v :)
  OpeningBracket closing opening -> modify (Bracket closing opening :) >> shuntingYard' ts
  ClosingBracket closing -> (++) <$> popUntilBracket closing <*> shuntingYard' ts
  Token.Operator operator -> case find (\a -> operator == opType a) buildInOperators of
    Nothing -> liftIO (putStrLn $ "unknown operator " ++ operator) >> lift (fail "")
    Just info -> (++) <$> popOperators info <*> shuntingYard' ts


popOperators :: OperatorInfo -> ShuntingYard [String]
popOperators info = get >>= go
  where
    go :: [OperatorStack] -> ShuntingYard [String]
    go [] = put [Operator info] >> return []
    go stack@(Bracket _ _ : _) = put (Operator info : stack) >> return []
    go stack@(Operator i : rest) = if opPrecedence i < opPrecedence info
      then put (Operator info : stack) >> return []
      else popOperators info


popUntilBracket :: Char -> ShuntingYard [String]
popUntilBracket bracket = get >>= go []
  where
    go :: [String] -> [OperatorStack] -> ShuntingYard [String]
    go _ [] = liftIO (putStrLn "missing bracket") >> lift (fail "")
    go output (Operator info : rest) = go (opType info : output) rest
    go output (Bracket closing _ : rest) = if bracket == closing
      then put rest >> return output
      else liftIO (putStrLn $ "missmatched brackets " ++ [bracket] ++ " and " ++ [closing]) >> lift (fail "")