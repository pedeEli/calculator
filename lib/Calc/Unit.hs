module Calc.Unit where

import Text.Parsec

import Data.List (findIndex)

import Control.Lens

import Calc.Types (SIUnit(..), Unit(..), Units, Token)

multiply :: Unit -> Unit -> Unit
multiply (Unit u1) (Unit u2) = Unit $ filter ((0 /=) . snd) $ go (u1 ++ u2) []
  where
    go :: Units -> Units -> Units
    go []            acc = acc
    go ((u, e) : us) acc = case findIndex ((== u) . fst) acc of
      Nothing -> go us ((u, e) : acc)
      Just index -> go us $ acc & ix index . _2 %~ (+e)

divide :: Unit -> Unit -> Unit
divide u1 (Unit u2) = multiply u1 $ Unit $ map (_2 %~ negate) u2


unitParser :: Parsec String (Maybe Token) Unit
unitParser = option (Unit []) $ do
  n <- choice [char '1' >> return [], many1 singleUnit]
  d <- option [] $ try $ char '/' >> many1 singleUnit
  return $ Unit $ n ++ map (_2 %~ negate) d


singleUnit :: Parsec String (Maybe Token) (SIUnit, Int)
singleUnit = do
  si <- siunit
  exponent <- option 1 $ char '^' >> read <$> many1 digit
  return (si, exponent)



siunit :: Parsec String (Maybe Token) SIUnit
siunit = choice [
  string "m" >> return Meter,
  string "s" >> return Second,
  string "kg" >> return Kilogram]