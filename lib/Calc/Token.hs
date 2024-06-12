module Calc.Token where

import Text.Parsec
import Text.Parsec.Prim (ParsecT(..))

import Data.List (singleton, intersperse)
import Data.Functor (($>), (<&>))
import Data.Functor.Identity (Identity)
import Data.Ratio (Ratio, (%))

import Control.Monad (when)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Calc.Unit (unit, empty, Unit(..))
import Calc.Value (Value(..))
import Debug.Trace (trace)

data Token =
  Token'Value Value |
  Token'Operator String |
  Token'OpeningBracket Char Char |
  Token'ClosingBracket Char
  deriving (Show)

data Cast = Cast [Token] String
  deriving (Show)

type Tokenizer = Parsec String ()

tokenize :: String -> MaybeT IO ([Token], Maybe Cast)
tokenize str = do
  let result = runParser temp () "" str
  case result of
    Left err -> liftIO (print err) >> fail ""
    Right tokens -> return tokens

temp :: Tokenizer ([Token], Maybe Cast)
temp = do
  ts <- Calc.Token.tokens
  cs <- optionMaybe cast
  return (ts, cs)

tokens :: Tokenizer [Token]
tokens = concat <$> many1 (spaces *> choice (implicitMult : singles) <* spaces)
  where
    singles = map (singleton <$>) [openingBracket, closingBracket, operator, rest]


implicitMult :: Tokenizer [Token]
implicitMult = do
  tokens <- many1 $ choice [unitWrapper, value]
  return $ intersperse (Token'Operator "*") tokens


number :: Tokenizer Rational
number = do
  digits <- many1 digit
  when (length digits /= 1 && head digits == '0') $ unexpected "0"

  decimal <- option "" decimalParser
  exponent <- option (1 % 1) exponentParser

  let numerator = read $ digits ++ decimal
      denominator = 10 ^ fromIntegral (length decimal)
      baseNumber = numerator % denominator
  return $ baseNumber * exponent

  where
    decimalParser = do
      char '.'
      many1 digit
    exponentParser = do
      e <- oneOf "eE"
      sign <- option "" (singleton <$> oneOf "+-")
      digits <- many1 digit
      return $ if sign == "-"
        then 1 % (10 ^ read digits)
        else (10 ^ read digits) % 1

unitWrapper :: Tokenizer Token
unitWrapper = do
  (u, r) <- unit
  return $ Token'Value $ Value r 1 u

value :: Tokenizer Token
value = do
  n <- number
  spaces
  (u, r) <- option empty unit
  return $ Token'Value $ Value (n * r) 1 u

openingBracket :: Tokenizer Token
openingBracket = choice [
  Token'OpeningBracket ')' <$> char '(',
  Token'OpeningBracket '}' <$> char '{']

closingBracket :: Tokenizer Token
closingBracket = Token'ClosingBracket <$> oneOf ")}"

operator :: Tokenizer Token
operator = Token'Operator <$> manyTill (noneOf "()[]{}") (lookAhead $ space <|> alphaNum)

cast :: Tokenizer Cast
cast = do
  char '['
  tokens <- lookAhead $ many1 units
  symbols <- manyTill anyChar (char ']')
  return $ Cast tokens symbols
  where
    value1, valueNot1 :: Tokenizer Token
    value1 = char '1' >> return (Token'Value $ Value 1 1 (Unit []))
    valueNot1 = oneOf "023456789" >> fail "only digit 1 is allowed"

    units = spaces *> choice [valueNot1, unitWrapper, value1, openingBracket, closingBracket, rest, operator] <* spaces

rest :: Tokenizer Token
rest = do
  l <- letter
  unexpected $ singleton l