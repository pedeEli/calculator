module Token (tokenize, Token(..)) where

import Text.Parsec

import Data.List (singleton)
import Data.Functor (($>))
import Data.Ratio (Ratio, (%))

import Control.Monad (when)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (MonadIO(liftIO))

data Token =
  Token'Value Rational |
  Token'Operator String |
  Token'OpeningBracket Char Char |
  Token'ClosingBracket Char
  deriving (Show)
type Tokenizer = ParsecT String () IO

tokenize :: String -> MaybeT IO [Token]
tokenize str = do
  result <- liftIO $ runParserT Token.token () "" str
  case result of
    Left err -> liftIO (print err) >> fail ""
    Right tokens -> return tokens


token :: Tokenizer [Token]
token = many $ spaces *> choice [rest, value, openingBracket, closingBracket, operator] <* spaces

value :: Tokenizer Token
value = do
  sign <- option "" (string "-")

  digits <- many1 digit
  when (length digits /= 1 && head digits == '0') $ unexpected "0"

  decimal <- option "" decimalParser
  exponent <- option (1 % 1) exponentParser

  let numerator = read $ sign ++ digits ++ decimal
      denominator = 10 ^ fromIntegral (length decimal)
      baseNumber = numerator % denominator
  return $ Token'Value $ baseNumber * exponent

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