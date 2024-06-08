module RPN (evaluate, RPN(..), OperatorInfo(..), buildInOperators) where

import Data.Ratio (Ratio, numerator, denominator)

import Unit (Unit, multiply, divide)

data OperatorInfo = OperatorInfo {
  opType :: String,
  opPrecedence :: Word, 
  fun :: Rational -> Rational -> Rational,
  fun2 :: Unit -> Unit -> Unit}

buildInOperators :: [OperatorInfo]
buildInOperators = [
  OperatorInfo "+" 0 (+) (const id),
  OperatorInfo "-" 0 (-) (const id),
  OperatorInfo "*" 1 (*) multiply,
  OperatorInfo "/" 1 (/) divide]

data RPN =
  RPN'Value Rational Unit |
  RPN'Operator OperatorInfo

instance Show RPN where
  show (RPN'Value d unit) = show d ++ show unit
  show (RPN'Operator info) = show $ opType info


evaluate :: [(Rational, Unit)] -> [RPN] -> String
evaluate []      [] = error "not possible"
evaluate ((d, u) : _) [] = showRational d ++ show u
evaluate ((d1, u1) : (d2, u2) : ds) (RPN'Operator info : rest) = evaluate ((fun info d2 d1, fun2 info u2 u1) : ds) rest
evaluate ds (RPN'Value d u : rest) = evaluate ((d, u) : ds) rest


showRational :: Rational -> String
showRational r =
  let n = numerator r
      d = denominator r
  in if d == 1
    then show n
    else show n ++ " / " ++ show d