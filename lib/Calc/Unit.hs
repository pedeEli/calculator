{-# LANGUAGE RecordWildCards, TemplateHaskell, TupleSections #-}
module Calc.Unit where

import Text.Parsec

import Data.List (findIndex, find, sort, sortBy, singleton)
import Data.Ord (Down(Down))

import Control.Lens


data SIUnit = Mass | Length | Time
  deriving (Eq, Ord)

type UnitList = [(SIUnit, Int)]
newtype Unit = Unit UnitList

$(makePrisms 'Unit)

instance Show SIUnit where
  show Length = "m"
  show Time   = "s"
  show Mass   = "kg"
instance Show Unit where
  show us = case find ((us ==) . view _2) composedUnits of
    Just (symbol, _, _) -> " " ++ symbol
    Nothing -> case splitAt0 us of
      ([] , [])  -> ""
      (pos, [])  -> " " ++ go pos
      ([] , neg) -> " 1/" ++ go (map (_2 *~ -1) neg)
      (pos, neg) -> " " ++ go pos ++ "/" ++ go (map (_2 *~ -1) neg)
    where
      go :: UnitList -> String
      go [] = ""
      go ((u, e) : us)
        | e == 1 = show u ++ go us
        | otherwise = show u ++ "^" ++ show e ++ go us
splitAt0 :: Unit -> (UnitList, UnitList)
splitAt0 = (both %~ sort) . foldl (\r u -> r & lens u %~ (u :)) ([], []) . view _Unit
  where lens u = if snd u > 0 then _1 else _2

instance Eq Unit where
  Unit ul1 == Unit ul2
    | length ul1 /= length ul2 = False
    | otherwise = sort ul1 == sort ul2

siUnit :: SIUnit -> Unit
siUnit unit = Unit [(unit, 1)]



multiply :: Unit -> Unit -> Unit
multiply (Unit u1) (Unit u2) = Unit $ filter ((0 /=) . snd) $ go (u1 ++ u2) []
  where
    go :: UnitList -> UnitList -> UnitList
    go [] acc = acc
    go ((u, e) : us) acc = case findIndex ((u ==) . view _1) acc of
      Nothing -> go us ((u, e) : acc)
      Just index -> go us $ acc & ix index . _2 +~ e

divide :: Unit -> Unit -> Unit
divide u1 u2 = multiply u1 $ u2 & _Unit . mapped . _2 *~ -1





empty :: (Unit, Rational)
empty = (Unit [], 1)

isEmpty :: (Unit, Rational) -> Bool
isEmpty (Unit unitList, _) = null unitList



unit :: Parsec String () (Unit, Rational)
unit = do
  (Unit unitList, r) <- singleUnit
  e <- option 1 $ char '^' >> read <$> many1 digit
  let uc = Unit $ map (_2 *~ e) unitList
  return (uc, r ^ e)


singleUnit :: Parsec String () (Unit, Rational)
singleUnit = choice $ map (\(s, a, r) -> try $ string s >> return (a, r)) allUnits

allUnits :: [(String, Unit, Rational)]
allUnits = sortBy (\(a, _, _) (b, _, _) -> compare (Down a) (Down b)) $ composedUnits ++ map (_2 %~ Unit . singleton . (,1)) siUnits

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

composedUnits :: [(String, Unit, Rational)]
composedUnits = [
  ("ha", Unit [(Length, 2)], 10000),
  ("Hz", Unit [(Time, -1)], 1),
  ("N",  Unit [(Mass, 1), (Length, 1), (Time, -2)], 1)]