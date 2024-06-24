module Calc.Token where

import Text.Parsec as P

import Data.List (singleton)
import Data.Ratio ((%))

import Control.Monad (when)
import Control.Monad.Trans.Except (Except, throwE)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Calc.Unit (unit, empty, Unit(..))
import Calc.Value (Value(..))
import Calc.Error (fromParsecError, Error, Position(..))


data TokenType =
  Token'Value Value |
  Token'Operator String |
  Token'OpeningBracket Char Char |
  Token'ClosingBracket Char
  deriving (Show)

data Token = Token {_tType :: TokenType, _tPos :: Position}

type Tokenizer = Parsec String ()

tokenize :: String -> Except Error ([Token], [Token])
tokenize str = do
  let result = runParser tokensAndCast () "" str
  case result of
    Right ts -> return ts
    Left err -> throwE $ fromParsecError err


addPosition :: Tokenizer TokenType -> Tokenizer Token
addPosition parser = do
  start <- getPosition
  token <- parser
  end <- getPosition
  return Token {_tType = token, _tPos = Position (sourceColumn start) (sourceColumn end)}


tokensAndCast :: Tokenizer ([Token], [Token])
tokensAndCast = do
  ts <- Calc.Token.tokens
  c <- option [] cast
  return (ts, c)

tokens :: Tokenizer [Token]
tokens = concat <$> many1 (spaces *> choice (implicitMult : singles) <* spaces)
  where
    singles = map (fmap singleton . addPosition) [openingBracket, closingBracket, operator]

implicitMult :: Tokenizer [Token]
implicitMult = do
  ts <- many1 $ choice $ map addPosition [unitWrapper, value]
  return $ intersperseWith f ts
  where
    f :: Token -> Token -> Token
    f t1 t2 = Token (Token'Operator "*") $ Position (_pStart $ _tPos t1) (_pEnd $ _tPos t2)


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

unitWrapper :: Tokenizer TokenType
unitWrapper = do
  (u, r, (symbol, _)) <- unit <?> "a unit"
  return $ Token'Value (Value r u (Unit []))

value :: Tokenizer TokenType
value = do
  n <- number
  (u, r, (symbol, _)) <- option empty $ try $ spaces *> unit
  return $ Token'Value (Value (n * r) u (Unit []))

openingBracket :: Tokenizer TokenType
openingBracket = choice [
  Token'OpeningBracket ')' <$> char '(',
  Token'OpeningBracket '}' <$> char '{']

closingBracket :: Tokenizer TokenType
closingBracket = Token'ClosingBracket <$> oneOf ")}"

operator :: Tokenizer TokenType
operator = Token'Operator <$> many1 (oneOf "^°!\"§$%&/?`´\\*+~'#,;.:-_<>|@€") <?> "operator"


cast :: Tokenizer [Token]
cast = do
  char '['
  ts <- many1 units
  char ']'
  return ts
  where
    value1, valueNot1, unitWrapper', operator' :: Tokenizer TokenType
    value1 = char '1' >> return (Token'Value (Value 1 (Unit []) (Unit [])))
    valueNot1 = oneOf "023456789" >> fail "only 1 is allowed"
    unitWrapper' = do
      (u, r, (symbol, e)) <- unit
      return $ Token'Value (Value r u (Unit [(symbol, e)]))
    operator' = choice [char '*' >> return (Token'Operator "*"), char '/' >> return (Token'Operator "/")]

    units = spaces *> choice (map addPosition [valueNot1, unitWrapper', value1, openingBracket, closingBracket, operator']) <* spaces





intersperseWith :: (a -> a -> a) -> [a] -> [a]
intersperseWith _ []  = []
intersperseWith _ [a] = [a]
intersperseWith f (a1 : a2 : rest) = a1 : f a1 a2 : intersperseWith f (a2 : rest)