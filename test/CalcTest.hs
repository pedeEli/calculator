{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.Providers (singleTest)
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Test.Tasty.QuickCheck (QC(QC))
import Test.QuickCheck

import Calc (calc)
import Calc.RPN (showRational)

import Utils (testCalc, convertToRational)

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [noUnits]


noUnits :: TestTree
noUnits = testGroup "No units" [
  $(testCalc "$a + $b" [| (+) |]),
  $(testCalc "$a - $b" [| (-) |]),
  $(testCalc "$a * $b" [| (*) |]),
  $(testCalc "$a / $/=0b" [| (/) |]),

  $(testCalc "$a+$>0b" [| (+) |]),
  $(testCalc "$a-$>0b" [| (-) |]),
  $(testCalc "$a*$>0b" [| (*) |]),
  $(testCalc "$a/$>0b" [| (/) |]),

  $(testCalc "$a + $b + $c" [| \a b c -> a + b + c |]),
  $(testCalc "$a - $b - $c" [| \a b c -> a - b - c |]),
  $(testCalc "$a * $b * $c" [| \a b c -> a * b * c |]),
  $(testCalc "$a / $/=0b / $/=0c" [| \a b c -> a / b / c |]),
  
  $(testCalc "$a + $b * $c" [| \a b c -> a + b * c |]),
  $(testCalc "$a * $b + $c" [| \a b c -> a * b + c |]),
  $(testCalc "($a + $b) * $c" [| \a b c -> (a + b) * c |]),
  $(testCalc "$a * ($b + $c)" [| \a b c -> a * (b + c) |])]