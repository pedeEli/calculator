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
  Meter |
  Second |
  Kilogram
  deriving (Eq)

instance Show SIUnit where
  show Meter = "m"
  show Second = "s"
  show Kilogram = "kg"


type Units = [(SIUnit, Int)]
showUnits :: Units -> String
showUnits [] = ""
showUnits ((u, e) : us)
  | e == 1 = show u ++ showUnits us
  | otherwise = show u ++ "^" ++ show e ++ showUnits us

newtype Unit = Unit Units
instance Show Unit where
  show (Unit us) = case splitAt0 us [] [] of
    ([] , [])  -> ""
    (pos, [])  -> showUnits pos
    ([] , neg) -> "1/" ++ showUnits neg
    (pos, neg) -> showUnits pos ++ "/" ++ showUnits (map (_2 %~ negate) neg)


splitAt0 :: Units -> Units -> Units -> (Units, Units)
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
    else show n ++ " / " ++ show d