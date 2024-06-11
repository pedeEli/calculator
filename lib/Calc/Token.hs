module Calc.Token where

import Text.Parsec

import Data.List (singleton)
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Data.Ratio (Ratio, (%))

import Control.Monad (when)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Calc.Unit (unit, empty)
import Calc.Value (Value(..))

data Token =
  Token'Value Value |
  Token'Operator String |
  Token'OpeningBracket Char Char |
  Token'ClosingBracket Char

type Tokenizer = Parsec String ()

tokenize :: String -> MaybeT IO [Token]
tokenize str = do
  let result = runParser Calc.Token.tokens () "" str
  case result of
    Left err -> liftIO (print err) >> fail ""
    Right tokens -> return tokens

tokens :: Tokenizer [Token]
tokens = many $ spaces *> choice [unitWrapper, value, openingBracket, closingBracket, operator] <* spaces

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
  Token'OpeningBracket ']' <$> char '[',
  Token'OpeningBracket '}' <$> char '}']

closingBracket :: Tokenizer Token
closingBracket = Token'ClosingBracket <$> oneOf ")]}"

operator :: Tokenizer Token
operator = Token'Operator <$> manyTill anyChar (lookAhead $ space <|> alphaNum <|> oneOf "()[]{}")

rest :: Tokenizer Token
rest = do
  l <- letter
  unexpected $ singleton l