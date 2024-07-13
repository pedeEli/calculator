{-# LANGUAGE TemplateHaskell #-}
module GCI.Types.UnitCreation where


import Data.Ratio
import Data.List

import Language.Haskell.TH


prefixes :: [(String, Integer)]
prefixes = [
  ("Q", 30),
  ("R", 27),
  ("Y", 24),
  ("Z", 21),
  ("E", 18),
  ("P", 15),
  ("T", 12),
  ("G", 9),
  ("M", 6),
  ("k", 3),
  ("h", 3),
  ("da", 2),
  ("d", -1),
  ("c", -2),
  ("m", -3),
  ("u", -6),
  ("n", -9),
  ("p", -12),
  ("f", -15),
  ("a", -18),
  ("z", -21),
  ("y", -24),
  ("r", -27),
  ("q", -30)]


createRational :: Integer -> ExpQ
createRational i
  | i >= 0 = litE $ rationalL $ fromIntegral $ 10 ^ i
  | otherwise = litE $ rationalL $ 1 % (10 ^ (-i))



convertUnitInfo :: UnitInfo -> [(String, [ExpQ])]
convertUnitInfo (unit, exp, Left r) = [(unit, [exp, litE $ rationalL r])]
convertUnitInfo (unit, exp, Right offset) = noPrefix : map go prefixes
  where
    noPrefix = (unit, [exp, createRational offset])
    go (prefix, i) = (prefix ++ unit, [exp, createRational $ offset + i])


convertToList :: [(String, [ExpQ])] -> ExpQ
convertToList = listE . map f . sortBy g
  where
    f :: (String, [ExpQ]) -> ExpQ
    f (unit, rest) = tupE $ litE (stringL unit) : rest

    g :: (String, [ExpQ]) -> (String, [ExpQ]) -> Ordering
    g (unit1, _) (unit2, _) = compare unit2 unit1

{-
Left: Do not add any prefixes
Right: Do add prefixes and apply offset
-}
type UnitInfo = (String, ExpQ, Either Rational Integer)

createDecs :: [UnitInfo] -> [UnitInfo] -> DecsQ
createDecs baseInfo composedInfo = sequenceA [
  sigD composedName unitsType,
  sigD allName unitsType,
  valD (varP composedName) (normalB composedUnits) [],
  valD (varP allName) (normalB allUnits) []
  ]

  where
    composedName = mkName "composedUnits"
    allName = mkName "allUnits"
    unitsType = [t| [(String, $(appT (conT $ mkName "Unit") (conT $ mkName "SIUnit")), Rational)] |]  

    composed = concatMap convertUnitInfo composedInfo
    base = concatMap convertUnitInfo baseInfo

    composedUnits = convertToList composed
    allUnits = convertToList $ composed ++ base