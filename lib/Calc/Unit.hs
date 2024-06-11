{-# LANGUAGE RecordWildCards #-}
module Calc.Unit where

import Text.Parsec

import Data.List (findIndex)

import Control.Lens

import Calc.Types
import Control.Monad (when)

import Debug.Trace

combineUnitLists :: UnitList -> UnitList -> UnitList
combineUnitLists u1 u2 = filter ((0 /=) . snd) $ go (u1 ++ u2) []
  where
    go :: UnitList -> UnitList -> UnitList
    go [] acc = acc
    go ((u, e) : us) acc = case findIndex ((u ==) . view _1) acc of
      Nothing -> go us ((u, e) : acc)
      Just index -> go us $ acc & ix index . _2 +~ e

multiply :: UnitComp -> UnitComp -> UnitComp
multiply u1 u2 = UnitComp (combineUnitLists (_ucSIUnits u1) (_ucSIUnits u2)) Nothing

divide :: UnitComp -> UnitComp -> UnitComp
divide u1 u2 = multiply u1 $ u2 & ucSIUnits . mapped . _2 *~ -1


multiply' :: (UnitComp, Rational) -> (UnitComp, Rational) -> (UnitComp, Rational)
multiply' (u1, r1) (u2, r2) = (
  multiply u1 u2,
  r1 * r2)

divide' :: (UnitComp, Rational) -> (UnitComp, Rational) -> (UnitComp, Rational)
divide' (u1, r1) (u2, r2) = (
  divide u1 u2,
  r1 / r2)


empty :: (UnitComp, Rational)
empty = (emptyUnitComp, 1)

isEmpty :: (UnitComp, Rational) -> Bool
isEmpty (uc, _) = null $ _ucSIUnits uc

unitParser :: Parsec String (Maybe Token) (UnitComp, Rational)
unitParser = option empty $ do
    n <- choice [char '1' >> return empty, unitList]
    d <- option empty $ try $ spaces >> char '/' >> spaces >> unitList
    when (isEmpty n && isEmpty d) $ unexpected "1"
    return $ divide' n d


unitList :: Parsec String (Maybe Token) (UnitComp, Rational)
unitList = do
  ul <- many1 unitWithExponent
  return $ foldr multiply' empty ul

unitWithExponent :: Parsec String (Maybe Token) (UnitComp, Rational)
unitWithExponent = do
  (UnitComp {..}, r) <- unit
  e <- option 1 $ char '^' >> read <$> many1 digit
  let uc = UnitComp {
    _ucSIUnits = map (_2 *~ e) _ucSIUnits,
    _ucSymbol = _ucSymbol & _Just . _2 *~ e}
  return (uc, r ^ e)



unit :: Parsec String (Maybe Token) (UnitComp, Rational)
unit = choice $ map (\(s, a, r) -> try $ string s >> return (a, r)) supportedUnits



supportedUnits :: [(String, UnitComp, Rational)]
supportedUnits = [
  ("min", unitComp Time,   60),
  ("um",  unitComp Length, 0.000001),
  ("mm",  unitComp Length, 0.001),
  ("cm",  unitComp Length, 10),
  ("dm",  unitComp Length, 100),
  ("km",  unitComp Length, 1000),
  ("kg",  unitComp Mass,   1),
  ("ha",  UnitComp [(Length, 2)] Nothing, 10000),
  ("h",   unitComp Time,   3600),
  ("s",   unitComp Time,   1),
  ("g",   unitComp Mass,   0.001),
  ("m",   unitComp Length, 1)]