module GCI.Parser.Lexer where


import qualified Calc.Value as V
import qualified Calc.Unit as U
import qualified Calc.Calculator as C
import qualified Calc.Error as E

import Text.Parsec

import Control.Monad

import Data.Ratio

import GCI.Types.SrcLoc




located :: Parsec String () a -> Parsec String () (Located a)
located p = do
  start <- getPosition
  token <- p
  end <- getPosition
  return $ mkLocated (sourceColumn start) (sourceColumn end) token

equal :: Parsec String () (Located Char)
equal = located $ char '='

bracketOpen :: Parsec String () (Located Char)
bracketOpen = located $ char '('

bracketClose :: Parsec String () (Located Char)
bracketClose = located $ char ')'

operator :: Parsec String () (Located String)
operator = located $ many1 (oneOf "^°!\"§%&/?`´\\*+~'#,;.:-_<>|@€")

variable :: Parsec String () (Located String)
variable = located $ do
  start <- letter
  rest <- many $ choice [alphaNum, oneOf "'_"]
  return (start : rest)

castOpen :: Parsec String () (Located Char)
castOpen = located $ char '['

castClose :: Parsec String () (Located Char)
castClose = located $ char ']'


value :: Parsec String () (Located Rational)
value = located $ do
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
      sign <- option "" ((:[]) <$> oneOf "+-")
      digits <- many1 digit
      return $ if sign == "-"
        then 1 % (10 ^ read digits)
        else (10 ^ read digits) % 1

singleUnit :: Parsec String () (Located (String, Maybe Int))
singleUnit = located $ do
  u <- choice $ map (\(str, _, _) -> try (string str)) U.allUnits
  e <- optionMaybe $ char '^' >> read <$> many1 digit
  return (u, e)