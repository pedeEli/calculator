{-# LANGUAGE RecordWildCards, TemplateHaskell, TupleSections, ScopedTypeVariables, TypeFamilies, OverloadedLists #-}
module Calc.Unit where

import GHC.Exts (IsList(..))
import Text.Parsec

import Data.List (findIndex, find, sort, sortBy, singleton)
import Data.Ord (Down(Down))

import Control.Lens


data SIUnit = Mass | Length | Time
  deriving (Eq, Ord)

type UnitList a = [(a, Integer)]
newtype Unit a = Unit (UnitList a)

$(makePrisms 'Unit)

instance IsList (Unit a) where
  type Item (Unit a) = (a, Integer)
  fromList = Unit
  toList = view _Unit


instance Show SIUnit where
  show Length = "m"
  show Time   = "s"
  show Mass   = "kg"

showUnit'String :: Unit String -> String
showUnit'String (Unit ul) = showUnitList id ul

showUnit'SIUnit :: Unit SIUnit -> String
showUnit'SIUnit us = case find (\a -> us == a ^. _2 && 1 == a ^. _3) composedUnits of
  Just (symbol, _, _) -> " " ++ symbol
  Nothing -> showUnitList show (us ^. _Unit)

showUnitList :: forall a. Ord a => (a -> String) -> UnitList a -> String
showUnitList toString ul = case splitAt0 ul of
  ([] , [])  -> ""
  (pos, [])  -> " " ++ go pos
  ([] , neg) -> " 1/" ++ go (map (_2 *~ -1) neg)
  (pos, neg) -> " " ++ go pos ++ "/" ++ go (map (_2 *~ -1) neg)
  where
    go :: UnitList a -> String
    go [] = ""
    go ((u, e) : us)
      | e == 1 = toString u ++ go us
      | otherwise = toString u ++ "^" ++ show e ++ go us

splitAt0 :: Ord a => UnitList a -> (UnitList a, UnitList a)
splitAt0 = (both %~ sort) . foldl (\r u -> r & getLens u %~ (u :)) ([], [])
  where getLens u = if snd u > 0 then _1 else _2

instance Ord a => Eq (Unit a) where
  Unit ul1 == Unit ul2
    | length ul1 /= length ul2 = False
    | otherwise = sort ul1 == sort ul2

siUnit :: SIUnit -> Unit SIUnit
siUnit u = Unit [(u, 1)]



multiply :: forall a. Eq a => Unit a -> Unit a -> Unit a
multiply (Unit u1) (Unit u2) = Unit $ filter ((0 /=) . snd) $ go (u1 ++ u2) []
  where
    go :: UnitList a -> UnitList a -> UnitList a
    go [] acc = acc
    go ((u, e) : us) acc = case findIndex ((u ==) . view _1) acc of
      Nothing -> go us ((u, e) : acc)
      Just i -> go us $ acc & ix i . _2 +~ e

divide :: Eq a => Unit a -> Unit a -> Unit a
divide u1 u2 = multiply u1 $ u2 & _Unit . mapped . _2 *~ -1





empty :: (Unit SIUnit, Rational, (String, Integer))
empty = ([], 1, ("", 1))

isEmpty :: (Unit SIUnit, Rational) -> Bool
isEmpty (Unit unitList, _) = null unitList



unit :: Parsec String () (Unit SIUnit, Rational, (String, Integer))
unit = do
  (symbol, Unit unitList, r) <- singleUnit
  e <- option 1 $ char '^' >> read <$> many1 digit
  let uc = Unit $ map (_2 *~ e) unitList
  return (uc, r ^ e, (symbol, e))


singleUnit :: Parsec String () (String, Unit SIUnit, Rational)
singleUnit = choice $ map (\u -> try (string $ u ^. _1) >> return u) allUnits

allUnits :: [(String, Unit SIUnit, Rational)]
allUnits = sortBy (\(a, _, _) (b, _, _) -> compare b a) $ composedUnits ++ map (_2 %~ Unit . singleton . (,1)) siUnits

siUnits :: [(String, SIUnit, Rational)]
siUnits = [
  ("min", Time,   60),
  ("ms",  Time,   0.001),
  ("um",  Length, 0.000001),
  ("mm",  Length, 0.001),
  ("cm",  Length, 10),
  ("dm",  Length, 100),
  ("km",  Length, 1000),
  ("kg",  Mass,   1),
  ("h",   Time,   3600),
  ("s",   Time,   1),
  ("g",   Mass,   0.001),
  ("m",   Length, 1)]

composedUnits :: [(String, Unit SIUnit, Rational)]
composedUnits = [
  ("ha", Unit [(Length, 2)], 10000),
  ("Hz", Unit [(Time, -1)], 1),
  ("N",  Unit [(Mass, 1), (Length, 1), (Time, -2)], 1)]