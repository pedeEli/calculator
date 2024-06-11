module Calc.RPN where

import Control.Monad.Trans.Maybe (MaybeT)

import Data.Ratio (Ratio, numerator, denominator)

import Calc.Unit (multiply, divide)
import Calc.Types (UnitComp, OperatorInfo(..), RPN(..), Result(..))
import Control.Monad.IO.Class (MonadIO(liftIO))

buildInOperators :: [OperatorInfo]
buildInOperators = [
  OperatorInfo "+" 0 (+) (const id),
  OperatorInfo "-" 0 (-) (const id),
  OperatorInfo "*" 1 (*) multiply,
  OperatorInfo "/" 1 (/) divide]


evaluate :: [RPN] -> MaybeT IO Result
evaluate = go []
  where
    go :: [(Rational, UnitComp)] -> [RPN] -> MaybeT IO Result
    go []       [] = error "not possible"
    go [(d, u)] [] = return $ Result d u
    go (_ : _)  [] = liftIO (putStrLn "missing operator") >> fail ""
    go ((d1, u1) : (d2, u2) : ds) (RPN'Operator info : rest) = go ((fun info d2 d1, fun2 info u2 u1) : ds) rest
    go ds (RPN'Value d u : rest) = go ((d, u) : ds) rest