module RPN where


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