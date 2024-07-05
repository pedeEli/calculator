{-# LANGUAGE TemplateHaskell #-}
module Calc.Token where


import Text.Parsec hiding (State)

import Data.List
import Data.Ratio
import Data.Maybe

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Lens

import qualified Calc.Unit as U
import qualified Calc.Error as E
import qualified Calc.Value as V
import qualified Calc.Calculator as C
import qualified Calc.Ast as Ast


data OperatorStackType =
  Op'Operator       E.Position String |
  Op'Variable       E.Position String Int |
  Op'OpeningBracket E.Position
  deriving (Show)
$(makePrisms ''OperatorStackType)

data State = State {_operatorStack :: [OperatorStackType], _outputStack :: [Ast.Ast V.Value], _isCast :: Bool}
  deriving (Show)
$(makeLenses 'State)

defaultState :: Bool -> State
defaultState isCast = State {_operatorStack = [], _outputStack = [], _isCast = isCast}

type ShuntingYard a = C.Calculator State a

(%:~) :: ASetter s t [a] [a] -> a -> s -> t
l %:~ a = l %~ (a :)

-- the boolean is used for negation
shuntingYard' :: Bool -> [Token] -> ShuntingYard ()
-- end of algorithm, everything thats still on the operator stack is pushed to output
shuntingYard' _ [] = C.get >>= shuntingYardEnd . _operatorStack
shuntingYard' start (t : ts) = case t of
  Token (Value r)           pos -> handleValue pos r ts
  Token (Unit u r symbol)   pos -> handleUnit pos u r symbol ts
  Token  OpeningBracket     pos -> handleOpeningBracket pos ts
  Token  ClosingBracket     pos -> handleClosingBracket pos ts
  Token (Operator operator) pos -> handleOperator start operator pos ts
  Token (Variable name)     pos -> handleVariable pos name ts

pushOperatorToOutput :: E.Position -> String -> ShuntingYard ()
pushOperatorToOutput pos name = do
  state <- C.get
  case _outputStack state of
    (a : b : rest) -> C.modify (outputStack .~ (Ast.Variable pos name [b, a] : rest))
    _ -> C.throwString pos "not enough argmuents"

pushVariableToOutput :: E.Position -> String -> Int -> ShuntingYard ()
pushVariableToOutput pos name i = do
  state <- C.get
  let (args, rest) = splitAt i (_outputStack state)
  C.modify (outputStack .~ Ast.Variable pos name (reverse args) : rest)


incrementStackHead :: ShuntingYard ()
incrementStackHead = C.modify (operatorStack . _head %~ go)
  where
    go :: OperatorStackType -> OperatorStackType
    go (Op'Variable pos name i) = Op'Variable pos name (i + 1)
    go op = op

handleVariable :: E.Position -> String -> [Token] -> ShuntingYard ()
handleVariable pos name ts = do
  state <- C.get
  when (_isCast state) $
    C.throwString pos "variables are not allowed in cast"
  go (_operatorStack state)
  shuntingYard' False ts
  where
    go :: [OperatorStackType] -> ShuntingYard ()
    go (Op'Variable pos _ _ : _) = do
      C.modify (outputStack %:~ Ast.Variable pos name [])
      incrementStackHead
    go _ = C.modify (operatorStack %:~ Op'Variable pos name 0)

handleClosingBracket :: E.Position -> [Token] -> ShuntingYard ()
handleClosingBracket pos ts = do
  state <- C.get
  go $ _operatorStack state
  shuntingYard' False ts
  where
    go :: [OperatorStackType] -> ShuntingYard ()
    go [] = C.throwString pos "missing opening bracket"
    go (Op'Operator pos name : rest) = do
      pushOperatorToOutput pos name
      go rest
    go (Op'Variable pos name i : rest) = do
      pushVariableToOutput pos name i
      go rest
    go (Op'OpeningBracket _ : rest) = C.modify (operatorStack .~ rest)

handleOpeningBracket :: E.Position -> [Token] -> ShuntingYard ()
handleOpeningBracket pos ts = do
  C.modify (operatorStack %:~ Op'OpeningBracket pos)
  shuntingYard' False ts

handleUnit :: E.Position -> U.Unit U.SIUnit -> Rational -> U.Unit String -> [Token] -> ShuntingYard ()
handleUnit pos u r symbol ts = do
  C.modify (outputStack %:~ Ast.Value pos (V.Value r u symbol))
  shuntingYard' False ts

handleValue :: E.Position -> Rational -> [Token] -> ShuntingYard ()
handleValue pos r ts = do
  state <- C.get
  when (_isCast state && r /= 1) $
    C.throwString pos "only number 1 is allowed in cast"
  let mapper = if _isCast state then id else V.stripUnitOverride
  C.modify (outputStack %:~ Ast.Value pos (mapper $ V.fromRational r))
  incrementStackHead
  shuntingYard' False ts


handleOperator :: Bool -> String -> E.Position -> [Token] -> ShuntingYard ()
handleOperator start name pos ts = do
  state <- C.get
  when (_isCast state && name `notElem` ["*", "/"]) $
    C.throwString pos "this operator is not allowed in cast"
  let isMinus = name == "-"
      stackHead = state ^? operatorStack . _head . _Op'OpeningBracket
  if isMinus && (start || isJust stackHead)
    then C.modify (operatorStack %:~ Op'Variable pos "negate" 0)
    else go (_operatorStack state)
  shuntingYard' False ts
  where
    go :: [OperatorStackType] -> ShuntingYard ()
    go [] = C.modify (operatorStack .~ [Op'Operator pos name])
    go stack@(Op'OpeningBracket _ : _) = C.modify (operatorStack .~ Op'Operator pos name : stack)
    go (Op'Variable pos name i : rest) = do
      pushVariableToOutput pos name i
      go rest
    go stack@(Op'Operator pos op : rest) = do
      p1 <- C.getPrecedence op
      p2 <- C.getPrecedence name
      if p1 < p2
        then C.modify (operatorStack .~ Op'Operator pos name : stack)
        else do
          pushOperatorToOutput pos op
          go rest

shuntingYardEnd :: [OperatorStackType] -> ShuntingYard ()
shuntingYardEnd [] = return ()
shuntingYardEnd (Op'OpeningBracket pos : _) = C.throwString pos "missing closing bracket"
shuntingYardEnd (Op'Operator pos name : rest) = do
  pushOperatorToOutput pos name
  shuntingYardEnd rest
shuntingYardEnd (Op'Variable pos name i : rest) = do
  pushVariableToOutput pos name i
  shuntingYardEnd rest

shuntingYard :: Bool -> [Token] -> C.Calculator a (Ast.Ast V.Value)
shuntingYard isCast tokens = do
  state <- C.replaceState (shuntingYard' True tokens >> C.get) (defaultState isCast)
  case _outputStack state of
    [ast] -> return ast
    rest -> C.throwString mempty "syntax error"




type Tokenizer = ParsecT String () (C.Calculator ())

mapTokenizer :: Tokenizer t -> String -> C.Calculator () t
mapTokenizer tokenizer str = do
  result <- runParserT (tokenizer <* eof) () "" str
  case result of
    Right t -> return t
    Left err -> throwE $ E.fromParsecError err str

expression :: String -> C.Calculator () (Ast.Ast V.Value)
expression = mapTokenizer $ expr

definition :: String -> C.Calculator () (String -> Ast.Ast ())
definition = mapTokenizer $ do
  (name, args) <- try operatorDef <|> variableDef
  spaces
  start <- getPosition
  char '='
  end <- getPosition
  spaces
  expr <- expr
  let pos = E.Position (sourceColumn start) (sourceColumn end)
  return $ \source -> Ast.Definition pos source name args expr
  where
    operatorDef :: Tokenizer (String, [String])
    operatorDef = do
      a <- variable
      spaces
      name <- operator
      spaces
      b <- variable
      return (name, [a, b])
    variableDef :: Tokenizer (String, [String])
    variableDef = do
        name <- variable
        spaces
        args <- many (variable <* spaces)
        return (name, args)

expr :: Tokenizer (Ast.Ast V.Value)
expr = do
  ts <- tokenize
  ast <- lift $ shuntingYard False ts
  maybeCast <- optionMaybe cast
  case maybeCast of
    Nothing -> return ast
    Just (pos, castValue) -> return $ Ast.Cast pos ast castValue

cast :: Tokenizer (E.Position, Ast.Ast V.Value)
cast = do
  start <- getPosition
  char '['
  ts <- tokenize
  char ']'
  end <- getPosition
  let pos = E.Position (sourceColumn start) (sourceColumn end)
  ast <- lift $ shuntingYard True ts
  return (pos, ast)




data TokenType =
  Value Rational |
  Unit (U.Unit U.SIUnit) Rational (U.Unit String) |
  Operator String |
  Variable String |
  OpeningBracket |
  ClosingBracket
  deriving (Show)

data Token = Token {_type :: TokenType, _pos :: E.Position}
  deriving (Show)

tokenize :: Tokenizer [Token]
tokenize = concat <$> many1 (spaces *> choice (implicitMult : singles) <* spaces)
  where
    singles = map (fmap singleton . addPosition) [
      openingBracket,
      closingBracket,
      operatorToken,
      variableToken]

addPosition :: Tokenizer TokenType -> Tokenizer Token
addPosition parser = do
  start <- getPosition
  token <- parser
  end <- getPosition
  return Token {_type = token, _pos = E.Position (sourceColumn start) (sourceColumn end)}

implicitMult :: Tokenizer [Token]
implicitMult = do
  ts <- many1 $ choice [unitWrapper, numberWrapper]
  return $ concat $ intersperseWith f ts
  where
    f :: [Token] -> [Token] -> [Token]
    f t1 t2 = [Token (Operator "*") $ E.Position (E._start $ _pos $ last t1) (E._end $ _pos $ head t2)]

    unitWrapper :: Tokenizer [Token]
    unitWrapper = fmap singleton $ addPosition $ do
      (u, r, symbol) <- unit
      return $ Unit u r $ U.Unit [symbol]

    numberWrapper :: Tokenizer [Token]
    numberWrapper = do
      p1 <- sourceColumn <$> getPosition
      n <- number
      p2 <- sourceColumn <$> getPosition
      unitMaybe <- optionMaybe unit
      p3 <- sourceColumn <$> getPosition
      case unitMaybe of
        Nothing -> return [Token (Value n) $ E.Position p1 p2]
        Just (u, r, symbol) -> return [
          Token OpeningBracket $ E.Position p1 p2,
          Token (Value n) $ E.Position p1 p2,
          Token (Operator "*") $ E.Position p1 p3,
          Token (Unit u r $ U.Unit [symbol]) $ E.Position p2 p3,
          Token ClosingBracket $ E.Position p2 p3]

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

variable :: Tokenizer String
variable = do
  char '$'
  start <- letter
  rest <- many $ choice [alphaNum, oneOf "'_"]
  return $ (start : rest)

variableToken :: Tokenizer TokenType
variableToken = Variable <$> variable

openingBracket :: Tokenizer TokenType
openingBracket = char '(' >> return OpeningBracket

closingBracket :: Tokenizer TokenType
closingBracket = char ')' >> return ClosingBracket

operator :: Tokenizer String
operator = many1 (oneOf "^°!\"§%&/?`´\\*+~'#,;.:-_<>|@€") <?> "operator"

operatorToken :: Tokenizer TokenType
operatorToken = Operator <$> operator

unit :: Tokenizer (U.Unit U.SIUnit, Rational, (String, Integer))
unit = do
  (symbol, U.Unit unitList, r) <- singleUnit <?> "a unit"
  e <- option 1 $ char '^' >> read <$> many1 digit
  let uc = U.Unit $ map (_2 *~ e) unitList
  return (uc, r ^ e, (symbol, e))
  where
    singleUnit :: Tokenizer (String, U.Unit U.SIUnit, Rational)
    singleUnit = choice $ map (\u -> try (string $ u ^. _1) >> return u) U.allUnits

intersperseWith :: (a -> a -> a) -> [a] -> [a]
intersperseWith _ []  = []
intersperseWith _ [a] = [a]
intersperseWith f (a1 : a2 : rest) = a1 : f a1 a2 : intersperseWith f (a2 : rest)