module Unit where

import Text.Parsec

import Data.List (findIndex)

import Control.Lens

data SIUnit =
  Meter |
  Second |
  Kilogram
  deriving (Eq)

instance Show SIUnit where
  show Meter = "m"
  show Second = "s"
  show Kilogram = "kg"


type Units = [(SIUnit, Int)]
showUnits :: Units -> String
showUnits [] = ""
showUnits ((u, e) : us)
  | e == 1 = show u ++ showUnits us
  | otherwise = show u ++ "^" ++ show e ++ showUnits us


newtype Unit = Unit Units
instance Show Unit where
  show (Unit us) = case splitAt0 us [] [] of
    ([] , [])  -> ""
    (pos, [])  -> showUnits pos
    ([] , neg) -> "1/" ++ showUnits neg
    (pos, neg) -> showUnits pos ++ "/" ++ showUnits (map (_2 %~ negate) neg)


splitAt0 :: Units -> Units -> Units -> (Units, Units)
splitAt0 [] pos neg = (pos, neg)
splitAt0 (u : us) pos neg = if snd u > 0
  then splitAt0 us (u : pos) neg
  else splitAt0 us pos (u : neg)



multiply :: Unit -> Unit -> Unit
multiply (Unit u1) (Unit u2) = go (u1 ++ u2) []
  where
    go :: Units -> Units -> Unit
    go [] acc = Unit acc
    go ((u, e) : us) acc = case findIndex ((== u) . fst) acc of
      Nothing -> go us ((u, e) : acc)
      Just index -> go us $ acc & ix index . _2 %~ (+e)

divide :: Unit -> Unit -> Unit
divide u1 (Unit u2) = multiply u1 $ Unit $ map (_2 %~ negate) u2


unitParser :: ParsecT String () IO Unit
unitParser = option (Unit []) $ do
  n <- choice [char '1' >> return [], many1 singleUnit]
  d <- option [] $ try $ char '/' >> many1 singleUnit
  return $ Unit $ n ++ map (_2 %~ negate) d


singleUnit :: ParsecT String () IO (SIUnit, Int)
singleUnit = do
  si <- siunit
  exponent <- option 1 $ char '^' >> read <$> many1 digit
  return (si, exponent)



siunit :: ParsecT String () IO SIUnit
siunit = choice [
  string "m" >> return Meter,
  string "s" >> return Second,
  string "kg" >> return Kilogram]