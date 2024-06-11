module Calc.Token where

import Text.Parsec

import Data.List (singleton)
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Data.Ratio (Ratio, (%))

import Control.Monad (when)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Calc.Unit (unitParser)
import Calc.Types (Token(..))

type Tokenizer = Parsec String (Maybe Token)

tokenize :: String -> MaybeT IO [Token]
tokenize str = do
  let result = runParser Calc.Token.tokens Nothing "" str
  case result of
    Left err -> liftIO (print err) >> fail ""
    Right tokens -> return tokens

tokens :: Tokenizer [Token]
tokens = many $ do
  spaces
  t <- choice [rest, value, openingBracket, closingBracket, operator]
  spaces
  putState $ Just t
  return t

number :: Tokenizer Rational
number = do
  lastToken <- getState
  when (isValue lastToken) $ fail ""
  sign <- option "" (string "-")

  digits <- many1 digit
  when (length digits /= 1 && head digits == '0') $ unexpected "0"

  decimal <- option "" decimalParser
  exponent <- option (1 % 1) exponentParser

  let numerator = read $ sign ++ digits ++ decimal
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

value :: Tokenizer Token
value = try $ do
  n <- number
  spaces
  (u, r) <- unitParser
  return $ Token'Value (n * r) u

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


isValue :: Maybe Token -> Bool
isValue (Just (Token'Value _ _)) = True
isValue _ = False