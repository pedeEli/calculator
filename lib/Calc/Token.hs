module Calc.Token where

import Text.Parsec as P

import Data.List (singleton, intersperse)
import Data.Ratio ((%))

import Control.Monad (when)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Calc.Unit (unit, empty, Unit(..))
import Calc.Value (Value(..))


data Token =
  Token'Value Value String |
  Token'Operator String |
  Token'OpeningBracket Char Char |
  Token'ClosingBracket Char
  deriving (Show)

type Tokenizer = Parsec String ()

tokenize :: String -> MaybeT IO ([Token], [Token])
tokenize str = do
  let result = runParser tokensAndCast () "" str
  case result of
    Left err -> liftIO (print err) >> fail ""
    Right ts -> return ts


tokensAndCast :: Tokenizer ([Token], [Token])
tokensAndCast = do
  ts <- Calc.Token.tokens
  c <- option [] cast
  return (ts, c)

tokens :: Tokenizer [Token]
tokens = concat <$> many1 (spaces *> choice (implicitMult : singles) <* spaces)
  where
    singles = map (singleton <$>) [openingBracket, closingBracket, rest, operator]

implicitMult :: Tokenizer [Token]
implicitMult = do
  ts <- many1 $ choice [unitWrapper, value]
  return $ intersperse (Token'Operator "*") ts


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

unitWrapper :: Tokenizer Token
unitWrapper = do
  (u, r, (symbol, e)) <- unit
  return $ Token'Value (Value r 1 u (Unit [])) (symbol ++ "^" ++ show e)

value :: Tokenizer Token
value = do
  n <- number
  spaces
  (u, r, (symbol, e)) <- option empty unit
  return $ Token'Value (Value (n * r) 1 u (Unit [])) (symbol ++ "^" ++ show e)

openingBracket :: Tokenizer Token
openingBracket = choice [
  Token'OpeningBracket ')' <$> char '(',
  Token'OpeningBracket '}' <$> char '{']

closingBracket :: Tokenizer Token
closingBracket = Token'ClosingBracket <$> oneOf ")}"

operator :: Tokenizer Token
operator = Token'Operator <$> manyTill (P.noneOf "()[]{}") (lookAhead $ space <|> alphaNum)

rest :: Tokenizer Token
rest = do
  l <- letter
  unexpected $ singleton l



cast :: Tokenizer [Token]
cast = do
  char '['
  ts <- many1 units
  char ']'
  return ts
  where
    value1, valueNot1, unitWrapper' :: Tokenizer Token
    value1 = char '1' >> return (Token'Value (Value 1 1 (Unit []) (Unit [])) "")
    valueNot1 = oneOf "023456789" >> fail "only 1 is allowed"
    unitWrapper' = do
      (u, r, (symbol, e)) <- unit
      return $ Token'Value (Value r 1 u (Unit [(symbol, e)])) symbol

    units = spaces *> choice [valueNot1, unitWrapper', value1, openingBracket, closingBracket, rest, operator] <* spaces