{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}
module Utils where


import Language.Haskell.TH

import Test.QuickCheck.Monadic as M

import Test.Tasty (TestName)

import Data.Maybe
import Data.List
import Data.Bifunctor (Bifunctor(second))

import Text.Parsec

import Control.Lens

import GCI.Parser.Lexer

import GCI.Types.SrcLoc
import GCI.Types.Value as V
import GCI.Calculator

runC :: String -> IO Interpret
runC str = startCalculator $ do
  addBuildIn2 "+" 0 (<<+>>)
  addBuildIn2 "-" 0 (<<->>)
  addBuildIn2 "*" 1 (<<*>>)
  addBuildIn2 "/" 1 (<</>>)
  addBuildIn2 "^" 2 (<<^>>)
  addBuildIn1 "negate" V.negate
  interpret str


testCalc :: String -> ExpQ -> String -> ExpQ
testCalc str f unit = do
  (vars, strings) <- parseExpr str
  let args = map createArg $ nub vars
      name = createName vars strings

  [| singleTest name $ QC $ property $ $(lamE args [| monadicIO $ do
    result <- M.run $ runC $ $(createCalc vars strings)
    case result of
      ValI str -> assert $ str == V.showRational ($(apply f vars)) ++ unit
      _ -> assert False |]) |]

  where
    typed v = [| $(varE $ mkName [vName v]) :: Double |]

    createArg :: Variable -> PatQ
    createArg v = case vMod v of
      Nothing ->  varP $ mkName [vName v]
      Just mod -> conP (mkName mod) [varP $ mkName [vName v]]

    createName :: [Variable] -> [String] -> String
    createName [] bs = concat bs
    createName (v : vs) (b : bs) = b ++ [vName v] ++ createName vs bs

    createCalc :: [Variable] -> [String] -> ExpQ
    createCalc [] bs = litE $ stringL $ concat bs
    createCalc (v : vs) (b : bs) = [| $(litE $ stringL b) ++ show $(typed v) ++ $(createCalc vs bs) |]

    apply :: ExpQ -> [Variable] -> ExpQ
    apply f [] = f
    apply f (v : vs) = apply [| $(f) (convertToRational $(varE $ mkName [vName v])) |] vs



convertToRational :: Double -> Rational
convertToRational = go . show
  where
    go :: String -> Rational
    go ('-' : rest) = -1 * go rest
    go rest =
      let Right v = runParser value () "" rest
      in unLoc v

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
parseMod = choice $ map try [
  string ">0" >> return "Positive",
  string "<0" >> return "Negative",
  string ">=0" >> return "NonNegative",
  string "<=0" >> return "NonPositive",
  string "/=0" >> return "NonZero"]
