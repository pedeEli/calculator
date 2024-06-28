{-# LANGUAGE OverloadedLists #-}
module Calc.Token where

import Text.Parsec as P

import Data.List (singleton)
import Data.Ratio ((%))

import Control.Monad (when)
import Control.Monad.Trans.Except (Except, throwE)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Calc.Unit (unit, empty, Unit(..), SIUnit)
import Calc.Error (fromParsecError, Error, Position(..))


data TokenType =
  Token'Value Rational |
  Token'Unit (Unit SIUnit) Rational (Unit String) |
  Token'Operator String |
  Token'OpeningBracket |
  Token'ClosingBracket |
  Token'CastStart |
  Token'CastEnd
  deriving (Show)

data Token = Token {_tType :: TokenType, _tPos :: Position}
  deriving (Show)

type Tokenizer = Parsec String ()

tokenize :: String -> Except Error [Token]
tokenize str = do
  let result = runParser Calc.Token.tokens () "" str
  case result of
    Right ts -> return ts
    Left err -> throwE $ fromParsecError err


addPosition :: Tokenizer TokenType -> Tokenizer Token
addPosition parser = do
  start <- getPosition
  token <- parser
  end <- getPosition
  return Token {_tType = token, _tPos = Position (sourceColumn start) (sourceColumn end)}


tokens :: Tokenizer [Token]
tokens = concat <$> many1 (spaces *> choice (implicitMult : singles) <* spaces) <* eof
  where
    singles = map (fmap singleton . addPosition) [openingBracket, closingBracket, castStart, castEnd, operator]

implicitMult :: Tokenizer [Token]
implicitMult = do
  ts <- many1 $ choice [unitWrapper, numberWrapper]
  return $ concat $ intersperseWith f ts
  where
    f :: [Token] -> [Token] -> [Token]
    f t1 t2 = [Token (Token'Operator "*") $ Position (_pStart $ _tPos $ last t1) (_pEnd $ _tPos $ head t2)]

    unitWrapper :: Tokenizer [Token]
    unitWrapper = fmap singleton $ addPosition $ do
      (u, r, symbol) <- unit
      return $ Token'Unit u r [symbol]

    numberWrapper :: Tokenizer [Token]
    numberWrapper = do
      p1 <- sourceColumn <$> getPosition
      n <- number
      p2 <- sourceColumn <$> getPosition
      unitMaybe <- optionMaybe unit
      p3 <- sourceColumn <$> getPosition
      case unitMaybe of
        Nothing -> return [Token (Token'Value n) $ Position p1 p2]
        Just (u, r, symbol) -> return [
          Token Token'OpeningBracket $ Position p1 p2,
          Token (Token'Value n) $ Position p1 p2,
          Token (Token'Operator "*") $ Position p1 p3,
          Token (Token'Unit u r [symbol]) $ Position p2 p3,
          Token Token'ClosingBracket $ Position p2 p3]


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


openingBracket :: Tokenizer TokenType
openingBracket = char '(' >> return Token'OpeningBracket

closingBracket :: Tokenizer TokenType
closingBracket = char ')' >> return Token'ClosingBracket

operator :: Tokenizer TokenType
operator = Token'Operator <$> many1 (oneOf "^°!\"§$%&/?`´\\*+~'#,;.:-_<>|@€") <?> "operator"

castStart :: Tokenizer TokenType
castStart = char '[' >> return Token'CastStart

castEnd :: Tokenizer TokenType
castEnd = char ']' >> return Token'CastEnd

{-
cast :: Tokenizer [Token]
cast = do
  char '['
  ts <- many1 units
  char ']'
  return ts
  where
    value1, valueNot1, unitWrapper', operator' :: Tokenizer TokenType
    value1 = char '1' >> return (Token'Value $ Value 1 [] [])
    valueNot1 = oneOf "023456789" >> fail "only 1 is allowed"
    unitWrapper' = do
      (u, r, (symbol, e)) <- unit
      return $ Token'Value (Value r u (Unit [(symbol, e)]))
    operator' = choice [char '*' >> return (Token'Operator "*"), char '/' >> return (Token'Operator "/")]

    units = spaces *> choice (map addPosition [valueNot1, unitWrapper', value1, openingBracket, closingBracket, operator']) <* spaces
-}





intersperseWith :: (a -> a -> a) -> [a] -> [a]
intersperseWith _ []  = []
intersperseWith _ [a] = [a]
intersperseWith f (a1 : a2 : rest) = a1 : f a1 a2 : intersperseWith f (a2 : rest)