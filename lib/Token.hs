module Token (tokenize) where

import Text.Parsec

import Data.List (singleton)
import Data.Functor (($>))

import Control.Monad (when)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (MonadIO(liftIO))


data Token =
  Value Double |
  Operator String |
  Bracket Char
  deriving (Show)
type Tokenizer = ParsecT String () IO

tokenize :: String -> MaybeT IO [Token]
tokenize str = do
  result <- liftIO $ runParserT Token.token () "" str
  case result of
    Left err -> liftIO (print err) >> fail ""
    Right tokens -> return tokens


token :: Tokenizer [Token]
token = many $ spaces *> choice [rest, value, bracket, operator] <* spaces

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

rest :: Tokenizer Token
rest = do
  l <- letter
  unexpected $ singleton l