{-# LANGUAGE OverloadedLists #-}
module Calc.Token where

import Text.Parsec hiding (tokens)

import Data.List
import Data.Ratio

import Control.Monad

import qualified Calc.Unit as U
import qualified Calc.Error as E
import qualified Calc.Calculator as C

data TokenType =
  Value Rational |
  Unit (U.Unit U.SIUnit) Rational (U.Unit String) |
  Operator String |
  Variable String |
  OpeningBracket |
  ClosingBracket |
  CastStart |
  CastEnd
  deriving (Show)

data Token = Token {_tType :: TokenType, _tPos :: E.Position}
  deriving (Show)

type Tokenizer = Parsec String ()

data Output =
  Expression [Token] |
  Definition [Token] [Token]


tokenize :: String -> C.Calculator () Output
tokenize str = do
  let result = runParser ((try definition <|> expression) <* eof) () "" str
  case result of
    Right ts -> return ts
    Left err -> C.throwError $ E.fromParsecError err


expression :: Tokenizer Output
expression = Expression <$> tokens

definition :: Tokenizer Output
definition = do
  leftSide <- tokens
  char '='
  rightSide <- tokens
  return $ Definition leftSide rightSide


addPosition :: Tokenizer TokenType -> Tokenizer Token
addPosition parser = do
  start <- getPosition
  token <- parser
  end <- getPosition
  return Token {_tType = token, _tPos = E.Position (sourceColumn start) (sourceColumn end)}


tokens :: Tokenizer [Token]
tokens = concat <$> many1 (spaces *> choice (implicitMult : singles) <* spaces)
  where
    singles = map (fmap singleton . addPosition) [openingBracket, closingBracket, castStart, castEnd, operator, variable]

implicitMult :: Tokenizer [Token]
implicitMult = do
  ts <- many1 $ choice [unitWrapper, numberWrapper]
  return $ concat $ intersperseWith f ts
  where
    f :: [Token] -> [Token] -> [Token]
    f t1 t2 = [Token (Operator "*") $ E.Position (E._pStart $ _tPos $ last t1) (E._pEnd $ _tPos $ head t2)]

    unitWrapper :: Tokenizer [Token]
    unitWrapper = fmap singleton $ addPosition $ do
      (u, r, symbol) <- U.unit
      return $ Unit u r [symbol]

    numberWrapper :: Tokenizer [Token]
    numberWrapper = do
      p1 <- sourceColumn <$> getPosition
      n <- number
      p2 <- sourceColumn <$> getPosition
      unitMaybe <- optionMaybe U.unit
      p3 <- sourceColumn <$> getPosition
      case unitMaybe of
        Nothing -> return [Token (Value n) $ E.Position p1 p2]
        Just (u, r, symbol) -> return [
          Token OpeningBracket $ E.Position p1 p2,
          Token (Value n) $ E.Position p1 p2,
          Token (Operator "*") $ E.Position p1 p3,
          Token (Unit u r [symbol]) $ E.Position p2 p3,
          Token ClosingBracket $ E.Position p2 p3]


number :: Tokenizer Rational
number = do
  digits <- many1 digit
  when (length digits /= 1 && head digits == '0') $ unexpected "0"

  decimal <- option "" decimalParser
  expo <- option (1 % 1) exponentParser

  let numerator = read $ digits ++ decimal
      denominator = 10 ^ fromIntegral (length decimal)
      baseNumber = numerator % denominator
  return $ baseNumber * expo

  where
    decimalParser = do
      char '.'
      many1 digit
    exponentParser = do
      oneOf "eE"
      sign <- option "" (singleton <$> oneOf "+-")
      digits <- many1 digit
      return $ if sign == "-"
        then 1 % (10 ^ read digits)
        else (10 ^ read digits) % 1

variable :: Tokenizer TokenType
variable = do
  char '$'
  start <- letter
  rest <- many $ choice [alphaNum, oneOf "'_"]
  return $ Variable (start : rest)

openingBracket :: Tokenizer TokenType
openingBracket = char '(' >> return OpeningBracket

closingBracket :: Tokenizer TokenType
closingBracket = char ')' >> return ClosingBracket

operator :: Tokenizer TokenType
operator = Operator <$> many1 (oneOf "^°!\"§%&/?`´\\*+~'#,;.:-_<>|@€") <?> "operator"

castStart :: Tokenizer TokenType
castStart = char '[' >> return CastStart

castEnd :: Tokenizer TokenType
castEnd = char ']' >> return CastEnd





intersperseWith :: (a -> a -> a) -> [a] -> [a]
intersperseWith _ []  = []
intersperseWith _ [a] = [a]
intersperseWith f (a1 : a2 : rest) = a1 : f a1 a2 : intersperseWith f (a2 : rest)