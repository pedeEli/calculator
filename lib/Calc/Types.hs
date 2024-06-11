{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
module Calc.Types where

import Control.Lens
import Data.Ratio (numerator, denominator)

data Token =
  Token'Value Rational UnitComp |
  Token'Operator String |
  Token'OpeningBracket Char Char |
  Token'ClosingBracket Char


data SIUnit =
  Length |
  Time |
  Mass
  deriving (Eq)

-- instance Eq Unit where
--   (==) :: Unit -> Unit -> Bool
--   u1 == u2 = _uSIUnit u1 == _uSIUnit u2
-- instance Ord Unit where
--   compare :: Unit -> Unit -> Ordering
--   compare u1 u2 = compare (_uFactor u1) (_uFactor u2)
instance Show SIUnit where
  show Length = "m"
  show Time   = "t"
  show Mass   = "kg"

type UnitList = [(SIUnit, Int)]
data UnitComp = UnitComp {_ucSIUnits :: UnitList, _ucSymbol :: Maybe (String, Int)}

emptyUnitComp :: UnitComp
emptyUnitComp = UnitComp [] Nothing

unitComp :: SIUnit -> UnitComp
unitComp unit = UnitComp {_ucSIUnits = [(unit, 1)], _ucSymbol = Nothing}

$(makeLenses 'UnitComp)


instance Show UnitComp where
  show (UnitComp _ (Just (s, e)))
    | e == 1    = s
    | e > 1     = s ++ "^" ++ show e
    | e == -1   = "1/" ++ s
    | otherwise = "1/" ++ s ++ "^" ++ show (-e)
  show (UnitComp us _) = case splitAt0 us of
    ([] , [])  -> ""
    (pos, [])  -> go pos
    ([] , neg) -> "/" ++ go (map (_2 *~ -1) neg)
    (pos, neg) -> go pos ++ "/" ++ go (map (_2 *~ -1) neg)
    where
      go :: UnitList -> String
      go [] = ""
      go ((u, e) : us)
        | e == 1 = show u ++ go us
        | otherwise = show u ++ "^" ++ show e ++ go us


splitAt0 :: UnitList -> (UnitList, UnitList)
splitAt0 = foldl (\r u -> r & lens u %~ (u :)) ([], [])
  where lens u = if snd u > 0 then _1 else _2




data OperatorInfo = OperatorInfo {
  opType :: String,
  opPrecedence :: Word, 
  fun :: Rational -> Rational -> Rational,
  fun2 :: UnitComp -> UnitComp -> UnitComp}

data RPN =
  RPN'Value Rational UnitComp |
  RPN'Operator OperatorInfo

instance Show RPN where
  show (RPN'Value d unit) = show d ++ show unit
  show (RPN'Operator info) = show $ opType info



data Result = Result Rational UnitComp

instance Show Result where
  show (Result r u) = showRational r ++ show u


showRational :: Rational -> String
showRational r =
  let n = numerator r
      d = denominator r
  in if d == 1
    then show n
    else show n ++ "/" ++ show d