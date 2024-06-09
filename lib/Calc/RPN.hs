module Calc.RPN where

import Data.Ratio (Ratio, numerator, denominator)

import Calc.Unit (multiply, divide)
import Calc.Types (Unit, OperatorInfo(..), RPN(..))

buildInOperators :: [OperatorInfo]
buildInOperators = [
  OperatorInfo "+" 0 (+) (const id),
  OperatorInfo "-" 0 (-) (const id),
  OperatorInfo "*" 1 (*) multiply,
  OperatorInfo "/" 1 (/) divide]


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