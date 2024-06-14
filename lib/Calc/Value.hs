{-# LANGUAGE TemplateHaskell #-}
module Calc.Value where


import Control.Lens

import Data.Ratio (numerator, denominator, (%))

import Calc.Unit (Unit(..), SIUnit, multiply, divide, _Unit, showUnit'SIUnit, showUnit'String)

data Value =
  Value {_vBase :: Rational, _vRoot :: Integer, _vUnit :: Unit SIUnit, _vUnitOverride :: Unit String} |
  Error String

$(makeLenses 'Value)

instance Show Value where
  show (Error s) = s
  show (Value b r u o) = showRoot r ++ showRational b ++ showUnit o u
    where
      showUnit :: Unit String -> Unit SIUnit -> String
      showUnit (Unit []) unit = showUnit'SIUnit unit
      showUnit unit      _    = showUnit'String unit


instance Num Value where
  Value b1 r1 u1 _ + Value b2 r2 u2 _
    | u1 /= u2 = Error "missmatching units"
    | otherwise = Value (applyRoot b1 r1 + applyRoot b2 r2) 1 u1 (Unit [])
  Error e + _ = Error e
  _       + e = e
  Value b1 r1 u1 o1 * Value b2 r2 u2 o2
    | r1 == r2  = Value (b1 * b2) r1 (multiply u1 u2) (multiply o1 o2)
    | otherwise = Value (applyRoot b1 r1 * applyRoot b2 r2) 1 (multiply u1 u2) (multiply o1 o2)
  Error e * _ = Error e
  _       * e = e
  abs = vBase %~ abs
  signum = (vBase %~ signum) . (vRoot .~ 1) . (vUnit .~ Unit [])
  fromInteger i = Value (fromInteger i) 1 (Unit []) (Unit [])
  negate = vBase *~ -1

instance Fractional Value where
  fromRational r = Value r 1 (Unit []) (Unit [])
  recip (Value b r u o) =
    let n = numerator b
        d = denominator b
    in Value (d ^ r * n ^ (r - 1) % n) r (divide (Unit []) u) (divide (Unit []) o)
  recip e = e


applyRoot :: Rational -> Integer -> Rational
applyRoot rational root
  | root == 1 = rational
  | otherwise =
    let n = numerator rational
        d = denominator rational
    in toRational (fromIntegral n ** (1 / fromIntegral root)) / fromIntegral d



showRational :: Rational -> String
showRational r =
  let n = numerator r
      d = denominator r
  in if d == 1
    then show n
    else show n ++ "/" ++ show d


showRoot :: Integer -> String
showRoot i
  | i == 1 = ""
  | otherwise = show i ++ "√"


isStandaloneUnit :: Value -> Bool
isStandaloneUnit (Error _) = False
isStandaloneUnit (Value v _ u _) = v == 1 && u ^. _Unit /= []