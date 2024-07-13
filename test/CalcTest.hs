{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.Providers
import Test.QuickCheck.Monadic as M
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Utils

import GCI.Types.Value as V

import GCI.Calculator


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [withoutUnits, withUnits, unitConversion, unitCast]


withoutUnits :: TestTree
withoutUnits = testGroup "Without units" [
  $(testCalc "$a + $>=0b" [| (+) |] ""),
  $(testCalc "$a - $>=0b" [| (-) |] ""),
  $(testCalc "$a * $>=0b" [| (*) |] ""),
  $(testCalc "$a / $>0b" [| (/) |] ""),

  $(testCalc "$a+$>=0b" [| (+) |] ""),
  $(testCalc "$a-$>=0b" [| (-) |] ""),
  $(testCalc "$a*$>=0b" [| (*) |] ""),
  $(testCalc "$a/$>0b" [| (/) |] ""),

  $(testCalc "$a + $>=0b + $>=0c" [| \a b c -> a + b + c |] ""),
  $(testCalc "$a - $>=0b - $>=0c" [| \a b c -> a - b - c |] ""),
  $(testCalc "$a * $>=0b * $>=0c" [| \a b c -> a * b * c |] ""),
  $(testCalc "$a / $>0b / $>0c" [| \a b c -> a / b / c |] ""),
  
  $(testCalc "$a + $>=0b * $>=0c" [| \a b c -> a + b * c |] ""),
  $(testCalc "$a * $>=0b + $>=0c" [| \a b c -> a * b + c |] ""),
  $(testCalc "($a + $>=0b) * $>=0c" [| \a b c -> (a + b) * c |] ""),
  $(testCalc "$a * ($b + $>=0c)" [| \a b c -> a * (b + c) |] "")]


withUnits :: TestTree
withUnits = testGroup "With units" [
  $(testCalc "$am + $>=0bm" [| (+) |] " m"),
  $(testCalc "$am - $>=0bm" [| (-) |] " m"),
  $(testCalc "$am * $>=0bm" [| (*) |] " m^2"),
  $(testCalc "$am / $>0bm" [| (/) |] ""),
  $(testCalc "$am/s^2" [| id |] " m/s^2"),
  $(testCalc "$as^2 / $>0bs" [| (/) |] " s")]


unitConversion :: TestTree
unitConversion = testGroup "Unit conversion" [
  $(testCalc "$akgm/s^2" [| id |] " N"),
  $(testCalc "$akgm/s^2 * 1s" [| id |] " kgm/s"),
  $(testCalc "$aN/kg" [| id |] " m/s^2"),
  $(testCalc "$akm" [| (* 1000) |] " m"),
  $(testCalc "$amm" [| (/ 1000) |] " m"),
  $(testCalc "$amin" [| (* 60) |] " s")]


unitCast :: TestTree
unitCast = testGroup "Unit cast" [
  $(testCalc "$ag [kg]" [| (/ 1000) |] " kg"),
  $(testCalc "$as[min]" [| (/ 60) |] " min"),
  $(testCalc "$am/s^2 [N/kg]" [| id |] " N/kg")]