{-# LANGUAGE TemplateHaskell, OverloadedLists #-}
module Calc.Value where


import Control.Lens
import Control.Monad.Trans.Except (Except, throwE)

import Data.Ratio (numerator, denominator, (%))

import Calc.Unit (Unit(..), SIUnit, multiply, divide, _Unit, showUnit'SIUnit, showUnit'String)

data Value = Value {_vBase :: Rational, _vUnit :: Unit SIUnit, _vUnitOverride :: Unit String}

$(makeLenses 'Value)

fromRational :: Rational -> Value
fromRational r = Value r [] []

stripUnitOverride :: Value -> Value
stripUnitOverride = vUnitOverride .~ []


instance Show Value where
  show (Value b u o) = showRational b ++ showUnit o u
    where
      showUnit :: Unit String -> Unit SIUnit -> String
      showUnit [] unit = showUnit'SIUnit unit
      showUnit unit      _    = showUnit'String unit


(<<+>>) :: Value -> Value -> Except String Value
Value b1 u1 _ <<+>> Value b2 u2 _
  | u1 /= u2  = throwE "missmatching units"
  | otherwise = return $ Value (b1 + b2) u1 []

(<<->>) :: Value -> Value -> Except String Value
Value b1 u1 _ <<->> Value b2 u2 _
  | u1 /= u2  = throwE "missmatching units"
  | otherwise = return $ Value (b1 - b2) u1 []

(<<*>>) :: Value -> Value -> Except String Value
Value b1 u1 o1 <<*>> Value b2 u2 o2 = return $
  Value (b1 * b2) (multiply u1 u2) (multiply o1 o2)

vNegate :: Value -> Except String Value
vNegate = return . (vBase *~ -1)

(<</>>) :: Value -> Value -> Except String Value
Value b1 u1 o1 <</>> Value b2 u2 o2 = return $ Value (b1 / b2) (divide u1 u2) (divide o1 o2)

vRecip :: Value -> Except String Value
vRecip (Value b u o) = return $ Value (recip b) (divide [] u) (divide [] o)


(<<^>>) :: Value -> Value -> Except String Value
Value b1 u1 o1 <<^>> Value b2 u2 _
  | u2 /= []           = throwE "exponent cannot have any units"
  | b1 == 0 && b2 == 0 = throwE "0^0 is undefined"
  | b2 == 0            = return $ Value 1 [] []
  | b2 == 1            = return $ Value b1 u1 o1
  | d2 == 1            = return $ Value (b1 ^ n2) (l u1) (l o1)
  | u1 /= []           = throwE "cannot take roots of units"
  | otherwise          = return $ Value (newtonsMethod (b1 ^ n2) d2) [] []
  where
    n2 = numerator   b2
    d2 = denominator b2
    l = _Unit . mapped . _2 *~ n2


showRational :: Rational -> String
showRational r = if denominator r == 1
  then show $ numerator r
  else show (Prelude.fromRational r :: Double)
  -- let n = numerator r
  --     d = denominator r
  -- in if d == 1
  --   then show n
  --   else show n ++ "/" ++ show d



newtonsMethod :: Rational -> Integer -> Rational
newtonsMethod a n = go 5 2
  where
    t1 = (n - 1) % n
    t2 = a / fromIntegral n

    go :: Int -> Rational -> Rational
    go 0 x = x
    go i x = go (i - 1) (t1 * x + t2 / (x ^ (n - 1)))