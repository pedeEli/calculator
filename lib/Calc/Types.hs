module Calc.Types where

import Control.Lens
import Data.Ratio (numerator, denominator)

data Token =
  Token'Value Rational Unit |
  Token'Operator String |
  Token'OpeningBracket Char Char |
  Token'ClosingBracket Char
  deriving (Show)


data SIUnit =
  Length |
  Time |
  Mass
  deriving (Eq)

data Unit = Unit {uSIUnit :: SIUnit, uFactor :: Rational, uSymbol :: String}
  deriving (Eq)
instance Show Unit where
  show = uSymbol

type UnitList = [(Unit, Int)]

data UnitComp = UnitComp {ucSIUnits :: UnitList, ucSymbol :: Maybe (String, Int)}

unitComp :: Unit -> UnitComp
unitComp unit = UnitComp {ucSIUnits = [(unit, 1)], ucSymbol = Nothing}


instance Show UnitComp where
  show (UnitComp _ (Just (s, e)))
    | e == 1    = s
    | e > 1     = s ++ "^" ++ show e
    | e == -1   = "1/" ++ s
    | otherwise = "1/" ++ s ++ "^" ++ show (-e)
  show (UnitComp us _) = case splitAt0 us [] [] of
    ([] , [])  -> ""
    (pos, [])  -> go pos
    ([] , neg) -> "1/" ++ go neg
    (pos, neg) -> go pos ++ "/" ++ go (map (_2 %~ negate) neg)
    where
      go :: UnitList -> String
      go [] = ""
      go ((u, e) : us)
        | e == 1 = show u ++ go us
        | otherwise = show u ++ "^" ++ show e ++ go us


splitAt0 :: UnitList -> UnitList -> UnitList -> (UnitList, UnitList)
splitAt0 [] pos neg = (pos, neg)
splitAt0 (u : us) pos neg = if snd u > 0
  then splitAt0 us (u : pos) neg
  else splitAt0 us pos (u : neg)




data OperatorInfo = OperatorInfo {
  opType :: String,
  opPrecedence :: Word, 
  fun :: Rational -> Rational -> Rational,
  fun2 :: Unit -> Unit -> Unit}

data RPN =
  RPN'Value Rational Unit |
  RPN'Operator OperatorInfo

instance Show RPN where
  show (RPN'Value d unit) = show d ++ show unit
  show (RPN'Operator info) = show $ opType info



data Result = Result Rational Unit

instance Show Result where
  show (Result r u) = showRational r ++ show u


showRational :: Rational -> String
showRational r =
  let n = numerator r
      d = denominator r
  in if d == 1
    then show n
    else show n ++ "/" ++ show d