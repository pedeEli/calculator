{-# LANGUAGE TemplateHaskell #-}
module Calc.Value where


import Control.Lens
import Control.Monad.Trans.Except (Except, throwE)

import Data.Ratio (numerator, denominator, (%))

import Calc.Unit (Unit(..), SIUnit, multiply, divide, _Unit, showUnit'SIUnit, showUnit'String)

data Value = Value {_vBase :: Rational, _vRoot :: Integer, _vUnit :: Unit SIUnit, _vUnitOverride :: Unit String}

$(makeLenses 'Value)

instance Show Value where
  show (Value b r u o) = showRoot r ++ showRational b ++ showUnit o u
    where
      showUnit :: Unit String -> Unit SIUnit -> String
      showUnit (Unit []) unit = showUnit'SIUnit unit
      showUnit unit      _    = showUnit'String unit


(<<+>>) :: Value -> Value -> Except String Value
Value b1 r1 u1 _ <<+>> Value b2 r2 u2 _
  | u1 /= u2  = throwE "missmatching units"
  | otherwise = return $ Value (applyRoot b1 r1 + applyRoot b2 r2) 1 u1 (Unit [])

(<<->>) :: Value -> Value -> Except String Value
Value b1 r1 u1 _ <<->> Value b2 r2 u2 _
  | u1 /= u2  = throwE "missmatching units"
  | otherwise = return $ Value (applyRoot b1 r1 - applyRoot b2 r2) 1 u1 (Unit [])

(<<*>>) :: Value -> Value -> Except String Value
Value b1 r1 u1 o1 <<*>> Value b2 r2 u2 o2
  | r1 == r2  = return $ Value (b1 * b2) r1 u o
  | otherwise = return $ Value (applyRoot b1 r1 * applyRoot b2 r2) 1 u o
  where
    u = multiply u1 u2
    o = multiply o1 o2

vNegate :: Value -> Except String Value
vNegate = return . (vBase *~ -1)

(<</>>) :: Value -> Value -> Except String Value
v1 <</>> v2 = vRecip v2 >>= (v1 <<*>>)

vRecip :: Value -> Except String Value
vRecip (Value b r u o) =
  let n = numerator b
      d = denominator b
  in return $ Value (d ^ r * n ^ (r - 1) % n) r (divide (Unit []) u) (divide (Unit []) o)


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