{-# LANGUAGE TemplateHaskell #-}
module Calc.Value where


import Control.Lens

import Data.Ratio (numerator, denominator, (%))

import Calc.Unit (Unit(..), multiply, divide, _Unit)

data Value =
  Value {_vBase :: Rational, _vRoot :: Integer, _vUnit :: Unit} |
  Error String

$(makeLenses 'Value)

instance Show Value where
  show (Error s) = s
  show (Value b r u) = showRoot r ++ showRational b ++ show u


instance Num Value where
  Value b1 r1 u1 + Value b2 r2 u2
    | u1 /= u2  = Error "missmatching units"
    | otherwise = Value (applyRoot b1 r1 + applyRoot b2 r2) 1 u1
  Error e + _ = Error e
  _       + e = e
  Value b1 r1 u1 * Value b2 r2 u2
    | r1 == r2  = Value (b1 * b2) r1 (multiply u1 u2)
    | otherwise = Value (applyRoot b1 r1 * applyRoot b2 r2) 1 (multiply u1 u2)
  Error e * _ = Error e
  _       * e = e
  abs = vBase %~ abs
  signum = (vBase %~ signum) . (vRoot .~ 1) . (vUnit .~ Unit [])
  fromInteger i = Value (fromInteger i) 1 (Unit []) 
  negate = vBase *~ -1

instance Fractional Value where
  fromRational r = Value r 1 (Unit [])
  recip (Value b r u) =
    let n = numerator b
        d = denominator b
    in Value (d ^ r * n ^ (r - 1) % n) r (divide (Unit []) u)
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
isStandaloneUnit (Value v _ u) = v == 1 && u ^. _Unit /= []