{-# LANGUAGE NamedFieldPuns #-}
module Utils where


import Language.Haskell.TH hiding (conE, litE, varE, varP, conT, conP)
import qualified Language.Haskell.TH as TH (conE, litE, varE, varP, conT, conP)

import Test.Tasty (TestName)

import Data.Maybe (isNothing)
import Data.List (nub, singleton, find)
import Data.Bifunctor (Bifunctor(second))

import Text.Parsec

import Control.Lens

import Calc.Token (number)


testCalc :: String -> ExpQ -> ExpQ
testCalc str f = do
  (vars, strings) <- parseExpr str
  let args = map createArg $ nub vars
      nameE = litE $ createName vars strings
      singleTestE = appE (varE "singleTest") nameE

  singleTestE $: conE "QC" $: varE "property" $: lamE args $ varE "monadicIO" $: doE [
    bindS (varP "result") $ varE "run" $: varE "calc" $: createCalc vars strings,
    noBindS $ caseE (varE "result") [
      match (conP "Nothing" []) (normalB $ appE (varE "fail") $ litE "") [],
      match (conP "Just" [conP "Result" [varP "r", varP "u"]]) (
        normalB $ varE "assert" $: varE "r" ==: (apply f vars)
      ) []
    ]]

  where
    showE = appE (varE "show")
    typed v = sigE (varE [vName v]) (conT "Double")

    createArg :: Variable -> PatQ
    createArg v = case vMod v of
      Nothing -> varP [vName v]
      Just mod -> conP mod [varP [vName v]]

    createName :: [Variable] -> [String] -> String
    createName [] bs = concat bs
    createName (v : vs) (b : bs) = b ++ [vName v] ++ createName vs bs

    createCalc :: [Variable] -> [String] -> ExpQ
    createCalc [] bs = litE $ concat bs
    createCalc (v : vs) (b : bs) = litE b ++: showE (typed v) ++: createCalc vs bs

    apply :: ExpQ -> [Variable] -> ExpQ
    apply f [] = f
    apply f (v : vs) = apply (appE f $ appE (varE "convertToRational") $ varE [vName v]) vs


-- utils ---------------------------------------
($:) :: Quote m => m Exp -> m Exp -> m Exp
e1 $: e2 = uInfixE e1 (varE "$") e2
infixr 0 $:
(++:) :: Quote m => m Exp -> m Exp -> m Exp
e1 ++: e2 = uInfixE e1 (varE "++") e2
infixr 5 ++:
(==:) :: Quote m => m Exp -> m Exp -> m Exp
e1 ==: e2 = uInfixE e1 (varE "==") e2
infix 4 ==:


conE :: Quote m => String -> m Exp
conE = TH.conE . mkName

varE :: Quote m => String -> m Exp
varE = TH.varE . mkName

litE :: Quote m => String -> m Exp
litE = TH.litE . stringL

varP :: Quote m => String -> m Pat
varP = TH.varP . mkName

conT :: Quote m => String -> m Type
conT = TH.conT . mkName

conP :: Quote m => String -> [m Pat] -> m Pat
conP = TH.conP . mkName


convertToRational :: Double -> Rational
convertToRational d = runParser number Nothing "" (show d) ^?! _Right


data Variable = Variable {vName :: Char, vMod :: Maybe String}
  deriving (Show, Eq)

parseExpr :: String -> Q ([Variable], [String])
parseExpr str = case runParser parser ([], []) "" str of
  Left err -> fail (show err)
  Right (vs, strings) -> if check vs
    then return (reverse vs, reverse strings)
    else fail "variables do not have the same modifiers"
  where
    parser = many (choice [parseVariable, parseRest]) >> getState
    check :: [Variable] -> Bool
    check [] = True
    check (v : vs) = case find ((vName v ==) . vName) vs of
      Nothing -> check vs
      Just r -> vMod r == vMod v && check vs


parseRest :: Parsec String ([Variable], [String]) ()
parseRest = do
  start <- Text.Parsec.noneOf "$"
  rest <- manyTill anyChar $ lookAhead (parseVariable <|> eof)
  modifyState $ second ((start : rest) :)

parseVariable :: Parsec String ([Variable], [String]) ()
parseVariable = do
  char '$'
  vMod <- optionMaybe parseMod
  vName <- letter
  let v = Variable { vName, vMod }
  modifyState $ \(vs, strings) -> if length vs == length strings
    then (v : vs, "" : strings)
    else (v : vs, strings)


parseMod :: Parsec String ([Variable], [String]) String
parseMod = choice [
  string ">0" >> return "Positive",
  string "<0" >> return "Negative",
  string ">=0" >> return "NonNegative",
  string "<=0" >> return "NonPositive",
  string "/=0" >> return "NonZero"]
