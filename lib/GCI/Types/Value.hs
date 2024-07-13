{-# LANGUAGE OverloadedLists #-}
module GCI.Types.Value where


import Control.Monad.Trans.Except

import Data.Ratio

import GCI.Types.Unit
import GCI.Types.SrcLoc


data Value = Value {
  base :: Rational,
  unit :: Unit SIUnit,
  unitOverride :: Unit String}

fromRational :: Rational -> Value
fromRational r = Value r [] []

fromUnit :: String -> Unit SIUnit -> Rational -> Maybe Integer -> Value
fromUnit str u r Nothing = Value r u [(str , 1)]
fromUnit str u r (Just e) = Value r (mapExp (* e) u) [(str, e)]

stripUnitOverride :: Value -> Value
stripUnitOverride (Value b u _) = Value b u []

unitsEqual :: Value -> Value -> Bool
unitsEqual (Value _ u1 _) (Value _ u2 _) = u1 == u2

castValue :: Value -> Value -> Value
castValue (Value b u _) (Value cb _ o) = Value (b / cb) u o



instance Show Value where
  show (Value b u o) = showRational b ++ showUnit o u
    where
      showUnit :: Unit String -> Unit SIUnit -> String
      showUnit []   unit = show unit
      showUnit unit _    = show unit


(<<+>>) :: Monad m => Value -> Value -> ExceptT (Located String) m Value
Value b1 u1 _ <<+>> Value b2 u2 _
  | u1 /= u2  = throwE $ L mempty "missmatching units"
  | otherwise = return $ Value (b1 + b2) u1 []

(<<->>) :: Monad m => Value -> Value -> ExceptT (Located String) m Value
Value b1 u1 _ <<->> Value b2 u2 _
  | u1 /= u2  = throwE $ L mempty "missmatching units"
  | otherwise = return $ Value (b1 - b2) u1 []

(<<*>>) :: Monad m => Value -> Value -> ExceptT (Located String) m Value
Value b1 u1 o1 <<*>> Value b2 u2 o2 = return $
  Value (b1 * b2) (multiply u1 u2) (multiply o1 o2)

negate :: Monad m => Value -> ExceptT (Located String) m Value
negate (Value b u o) = return $ Value (-b) u o

(<</>>) :: Monad m => Value -> Value -> ExceptT (Located String) m Value
Value b1 u1 o1 <</>> Value b2 u2 o2 = return $ Value (b1 / b2) (divide u1 u2) (divide o1 o2)

vRecip :: Monad m => Value -> ExceptT (Located String) m Value
vRecip (Value b u o) = return $
  Value (recip b) (divide [] u) (divide [] o)


(<<^>>) :: Monad m => Value -> Value -> ExceptT (Located String) m Value
Value b1 u1 o1 <<^>> Value b2 u2 _
  | u2 /= []           = throwE $ L mempty "exponent cannot have any units"
  | b1 == 0 && b2 == 0 = throwE $ L mempty "0^0 is undefined"
  | b2 == 0            = return $ Value 1 [] []
  | b2 == 1            = return $ Value b1 u1 o1
  | d2 == 1            = return $ Value (b1 ^ n2) (f u1) (f o1)
  | u1 /= []           = throwE $ L mempty "cannot take roots of units"
  | otherwise          = return $ Value (newtonsMethod (b1 ^ n2) d2) [] []
  where
    n2 = numerator   b2
    d2 = denominator b2
    f = mapExp (* n2)


showRational :: Rational -> String
showRational r = if denominator r == 1
  then show $ numerator r
  else show (Prelude.fromRational r :: Double)



newtonsMethod :: Rational -> Integer -> Rational
newtonsMethod a n = go 5 2
  where
    t1 = (n - 1) % n
    t2 = a / fromIntegral n

    go :: Int -> Rational -> Rational
    go 0 x = x
    go i x = go (i - 1) (t1 * x + t2 / (x ^ (n - 1)))