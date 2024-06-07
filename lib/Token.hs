module Token where

import Text.Parsec
import Control.Monad (when)
import Data.List (singleton)



tokenize :: String -> [String]
tokenize str = undefined


data Token =
  Value Double |
  Operator String |
  Bracket Char
  deriving (Show)
type Tokenizer = ParsecT String () IO

tokenizer :: Tokenizer [Token]
tokenizer = many $ spaces *> choice [value, bracket, operator] <* spaces


value :: Tokenizer Token
value = do
  sign <- option "" (string "-")

  digits <- many1 digit
  when (length digits /= 1 && head digits == '0') $ unexpected "0"

  decimal <- option "" decimalParser
  exponent <- option "" exponentParser

  return $ Value $ read $ sign ++ digits ++ decimal ++ exponent

  where
    decimalParser = do
      char '.'
      digits <- many1 digit
      return ('.' : digits)
    exponentParser = do
      e <- oneOf "eE"
      sign <- option "" (singleton <$> oneOf "+-")
      digits <- many1 digit
      return ('e' : sign ++ digits)

brackets = oneOf "()[]{}"
bracket :: Tokenizer Token
bracket = Bracket <$> brackets

operator :: Tokenizer Token
operator = Operator <$> manyTill anyChar (lookAhead $ space <|> alphaNum <|> brackets)