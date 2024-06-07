module RPN (evaluate, RPN(..), OperatorInfo(..), buildInOperators) where


data OperatorInfo = OperatorInfo {opType :: String, opPrecedence :: Word, fun :: Double -> Double -> Double}

buildInOperators :: [OperatorInfo]
buildInOperators = [
  OperatorInfo "+" 0 (+),
  OperatorInfo "*" 1 (*),
  OperatorInfo "^" 2 (**)]

data RPN =
  RPN'Value Double |
  RPN'Operator OperatorInfo

instance Show RPN where
  show (RPN'Value d) = show d
  show (RPN'Operator info) = show $ opType info


evaluate :: [Double] -> [RPN] -> Double
evaluate []      [] = error "not possible"
evaluate (d : _) [] = d
evaluate (d1 : d2 : ds) (RPN'Operator info : rest) = evaluate (fun info d1 d2 : ds) rest
evaluate ds (RPN'Value d : rest) = evaluate (d : ds) rest