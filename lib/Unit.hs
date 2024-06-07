{-# LANGUAGE InstanceSigs #-}
module Unit where


import Text.Parsec

data SIUnit =
  Meter |
  Second |
  Kilogram

instance Show SIUnit where
  show Meter = "m"
  show Second = "s"
  show Kilogram = "kg"


newtype UnitMult = UnitMult [(SIUnit, Word)]
instance Show UnitMult where
  show (UnitMult us) = go us
    where
      go :: [(SIUnit, Word)] -> String
      go [] = ""
      go ((u, e) : rest) = if e == 1
        then show u ++ go rest
        else show u ++ "^" ++ show e ++ go rest


data Unit = Unit UnitMult UnitMult
instance Show Unit where
  show (Unit (UnitMult []) (UnitMult [])) = ""
  show (Unit n             (UnitMult [])) = show n
  show (Unit (UnitMult []) d)             = "1/" ++ show d
  show (Unit n             d)             = show n ++ "/" ++ show d


unitParser :: ParsecT String () IO Unit
unitParser = option (Unit (UnitMult []) (UnitMult [])) $ do
  n <- unitMult
  d <- option (UnitMult []) $ char '/' >> unitMult
  return $ Unit n d


unitMult :: ParsecT String () IO UnitMult
unitMult = choice [
  char '1' >> return (UnitMult []),
  UnitMult <$> many1 singleUnit]


singleUnit :: ParsecT String () IO (SIUnit, Word)
singleUnit = do
  si <- siunit
  exponent <- option 1 $ char '^' >> read <$> many1 digit
  return (si, exponent)



siunit :: ParsecT String () IO SIUnit
siunit = choice [
  string "m" >> return Meter,
  string "s" >> return Second,
  string "kg" >> return Kilogram]