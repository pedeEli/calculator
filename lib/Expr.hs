module Expr where

import Token (Token(Value, OpeningBracket, ClosingBracket))
import qualified Token (Token(Operator))

import Data.List (singleton, find)

data Expr =
  Mult Expr Expr |
  Add Expr Expr |
  Power Expr Expr |
  Val Double


data OperatorInfo = OperatorInfo {opType :: String, opPrecedence :: Word}

operators :: [OperatorInfo]
operators = [
  OperatorInfo "+" 0,
  OperatorInfo "*" 1,
  OperatorInfo "^" 2]


data OperatorStack = Operator OperatorInfo | Bracket Char Char

shuntingYard :: [Token] -> [OperatorStack] -> [String]
shuntingYard [] [] = []
shuntingYard [] (Bracket closing _:_) = error $ "missing bracket " ++ singleton closing
shuntingYard [] (Operator i:ops) = opType i : shuntingYard [] ops
shuntingYard (t:ts) ops = case t of
  Value v -> show v : shuntingYard ts ops
  Token.Operator op -> case find (\a -> op == opType a) operators of
    Nothing -> error $ "unknown operator " ++ op
    Just info ->
      let (output, rest) = popOperators info ops
      in output ++ shuntingYard ts rest
  OpeningBracket closing opening -> shuntingYard ts (Bracket closing opening : ops)
  ClosingBracket b ->
    let (output, rest) = popUntilBracket b ops
    in output ++ shuntingYard ts rest


popOperators :: OperatorInfo -> [OperatorStack] -> ([String], [OperatorStack])
popOperators info [] = ([], [Operator info])
popOperators info ops@(Bracket _ _:_) = ([], Operator info : ops)
popOperators info os@(Operator i:ops) = if opPrecedence i >= opPrecedence info
  then
    let (output, rest) = popOperators info ops
    in (opType info : output, rest)
  else ([], Operator info : os)



popUntilBracket :: Char -> [OperatorStack] -> ([String], [OperatorStack])
popUntilBracket _ [] = error "missing bracket"
popUntilBracket b (Operator o:ops) =
  let (output, remaining) = popUntilBracket b ops
  in (opType o : output, remaining)
popUntilBracket bracket (Bracket closing _:ops) = if bracket == closing
  then ([], ops)
  else error $ "missmatched brackets " ++ singleton bracket ++ " and " ++ singleton closing