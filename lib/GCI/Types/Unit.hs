{-# LANGUAGE RecordWildCards, TemplateHaskell, TupleSections, ScopedTypeVariables,
    TypeFamilies, OverloadedLists, FlexibleInstances #-}
{-# OPTIONS_GHC -ddump-splices #-}
module GCI.Types.Unit where


import GHC.Exts

import Data.List

import GCI.Types.UnitCreation


data SIUnit = Mass | Length | Time
  deriving (Eq, Ord)

instance Show SIUnit where
  show Length = "m"
  show Time   = "s"
  show Mass   = "kg"


type UnitList a = [(a, Integer)]
newtype Unit a = Unit (UnitList a)

instance IsList (Unit a) where
  type Item (Unit a) = (a, Integer)
  fromList = Unit
  toList (Unit ul) = ul

mapExp :: (Integer -> Integer) -> Unit a -> Unit a
mapExp f (Unit ul) = Unit $ mapExp' f ul

mapExp' :: (Integer -> Integer) -> UnitList a -> UnitList a
mapExp' f = map $ fmap f


$(createDecs [
    ("min", [| [(Time, 1)] |],   Left 60),
    ("h",   [| [(Time, 1)] |],   Left 3600),
    ("d",   [| [(Time, 1)] |],   Left 86400),
    ("day", [| [(Time, 1)] |],   Left 86400),
    ("m",   [| [(Length, 1)] |], Right   0),
    ("s",   [| [(Time, 1)] |],   Right   0),
    ("g",   [| [(Mass, 1)] |],   Right (-3)),
    ("t",   [| [(Mass, 1)] |],   Right   3),
    ("l",   [| [(Length, 3)] |], Right (-3))
  ] [
    ("ha",  [| [(Length, 2)] |],                         Left 10000),
    ("a",   [| [(Length, 2)] |],                         Left 100),
    ("Pa",  [| [(Mass, 1), (Length, -1), (Time, -2)] |], Left 1),
    ("hPa", [| [(Mass, 1), (Length, -1), (Time, -2)] |], Left 100),
    ("Hz",  [| [(Time, -1)] |],                          Right 0),
    ("N",   [| [(Mass, 1), (Length, 1), (Time, -2)] |],  Right 0),
    ("J",   [| [(Mass, 1), (Length, 2), (Time, -2)] |],  Right 0),
    ("W",   [| [(Mass, 1), (Length, 2), (Time, -3)] |],  Right 0)
  ])


instance Show (Unit String) where
  show (Unit ul) = showUnitList id ul

instance Show (Unit SIUnit) where
  show us = case find (\(_, u, r) -> us == u && 1 == r) composedUnits of
    Just (symbol, _, _) -> " " ++ symbol
    Nothing -> showUnitList show (toList us)

showUnitList :: forall a. Ord a => (a -> String) -> UnitList a -> String
showUnitList toString ul = case splitAt0 ul of
  ([] , [])  -> ""
  (pos, [])  -> " " ++ go pos
  ([] , neg) -> " 1/" ++ go (mapExp' negate neg)
  (pos, neg) -> " " ++ go pos ++ "/" ++ go (mapExp' negate neg)
  where
    go :: UnitList a -> String
    go [] = ""
    go ((u, e) : us)
      | e == 1 = toString u ++ go us
      | otherwise = toString u ++ "^" ++ show e ++ go us

splitAt0 :: forall a. Ord a => UnitList a -> (UnitList a, UnitList a)
splitAt0 = sort' . foldl f ([], [])
  where
    sort' :: (UnitList a, UnitList a) -> (UnitList a, UnitList a)
    sort' (pos, neg) = (sort pos, sort neg)
    f :: (UnitList a, UnitList a) -> (a, Integer) -> (UnitList a, UnitList a)
    f (pos, neg) (a, e)
      | e < 0     = (pos, (a, e) : neg)
      | otherwise = ((a, e) : pos, neg)

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
    go ((u, e) : us) acc = go us $ f u e acc

    f :: a -> Integer -> UnitList a -> UnitList a
    f u e [] = [(u, e)]
    f u e ((u', e') : us) = if u == u'
      then (u', e' + e) : us
      else (u', e') : f u e us

divide :: Eq a => Unit a -> Unit a -> Unit a
divide u1 u2 = multiply u1 $ mapExp negate u2



empty :: (Unit SIUnit, Rational, (String, Integer))
empty = ([], 1, ("", 1))

isEmpty :: (Unit SIUnit, Rational) -> Bool
isEmpty (Unit unitList, _) = null unitList