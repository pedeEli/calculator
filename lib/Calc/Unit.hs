{-# LANGUAGE RecordWildCards #-}
module Calc.Unit where

import Text.Parsec

import Data.List (findIndex)

import Control.Lens

import Calc.Types

multiply = undefined
divide = undefined

combineUnitLists :: UnitList -> UnitList -> UnitList
combineUnitLists u1 u2 = filter ((0 /=) . snd) $ go (u1 ++ u2) []
  where
    go :: UnitList -> UnitList -> UnitList
    go []            acc = acc
    go ((u, e) : us) acc = case findIndex ((== u) . fst) acc of
      Nothing -> go us ((u, e) : acc)
      Just index -> go us $ acc & ix index . _2 %~ (+e)

divide' :: UnitList -> UnitList -> UnitList
divide' u1 u2 = combineUnitLists u1 $ map (_2 %~ negate) u2



unitParser :: Parsec String (Maybe Token) UnitComp
unitParser = option (UnitComp [] Nothing) $ do
  n <- choice [char '1' >> return [], many1 unitWithExponent]
  d <- option [] $ try $ char '/' >> many1 unitWithExponent
  undefined
-- unitParser = option (UnitComp [] Nothing) $ do
--   n <- choice [char '1' >> return [], many1 singleUnit]
--   d <- option [] $ try $ char '/' >> many1 singleUnit
--   return $ Unit $ n ++ map (_2 %~ negate) d


unitWithExponent :: Parsec String (Maybe Token) UnitComp
unitWithExponent = do
  UnitComp {..} <- unit
  e <- option 1 $ char '^' >> read <$> many1 digit
  return $ UnitComp {
    ucSIUnits = map (_2 *~ e) ucSIUnits,
    ucSymbol = ucSymbol & _Just . _2 *~ e}



unit :: Parsec String (Maybe Token) UnitComp
unit = choice [
  string "min" <&> unitComp . Unit Time   60,
  string "mum" <&> unitComp . Unit Length 0.001,
  string "mm"  <&> unitComp . Unit Length 0.001,
  string "cm"  <&> unitComp . Unit Length 10,
  string "dm"  <&> unitComp . Unit Length 100,
  string "km"  <&> unitComp . Unit Length 1000,
  string "kg"  <&> unitComp . Unit Mass   1,
  string "h"   <&> unitComp . Unit Time   3600,
  string "s"   <&> unitComp . Unit Time   1,
  string "g"   <&> unitComp . Unit Mass   1,
  string "m"   <&> unitComp . Unit Length 1]