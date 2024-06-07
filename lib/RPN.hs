module RPN (evaluate, RPN(..), OperatorInfo(..), buildInOperators) where

import Data.Ratio (Ratio, numerator, denominator)

data OperatorInfo = OperatorInfo {
  opType :: String,
  opPrecedence :: Word, 
  fun :: Rational -> Rational -> Rational}

buildInOperators :: [OperatorInfo]
buildInOperators = [
  OperatorInfo "+" 0 (+),
  OperatorInfo "-" 0 (-),
  OperatorInfo "*" 1 (*),
  OperatorInfo "/" 1 (/)]

data RPN =
  RPN'Value Rational |
  RPN'Operator OperatorInfo

instance Show RPN where
  show (RPN'Value d) = show d
  show (RPN'Operator info) = show $ opType info


evaluate :: [Rational] -> [RPN] -> String
evaluate []      [] = error "not possible"
evaluate (d : _) [] = showRational d
evaluate (d1 : d2 : ds) (RPN'Operator info : rest) = evaluate (fun info d2 d1 : ds) rest
evaluate ds (RPN'Value d : rest) = evaluate (d : ds) rest


showRational :: Rational -> String
showRational r =
  let n = numerator r
      d = denominator r
  in if d == 1
    then show n
    else show n ++ " / " ++ show d