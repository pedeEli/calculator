{-# LANGUAGE TemplateHaskell, ExistentialQuantification, FlexibleContexts #-}
module Calc.ShuntingYard where

import Data.List
import Data.Maybe

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Except

import qualified Calc.Error as E
import qualified Calc.Token as T
import qualified Calc.Value as V
import qualified Calc.RPN as RPN
import qualified Calc.Calculator as C

import Debug.Trace


data OperatorStackType =
  Operator E.Position String |
  forall f. C.Fun f => Function E.Position String f |
  OpeningBracket E.Position Char
instance Show OperatorStackType where
  show (Operator _ name) = "Operator " ++ name
  show (Function _ name _) = "Function " ++ name
  show (OpeningBracket _ b) = "OpeningBracket " ++ [b]

$(makePrisms ''OperatorStackType)

type Output = ([RPN.RPN], Maybe [RPN.RPN])

data State = State {_stack :: [OperatorStackType], _isCast :: Bool}
  deriving (Show)
$(makeLenses 'State)

defaultState :: State
defaultState = State {_stack = [], _isCast = False}

type ShuntingYard a = C.Calculator State a


(%:~) :: ASetter s t [a] [a] -> a -> s -> t
l %:~ a = l %~ (a :)


addToOutput :: Bool -> [ShuntingYard RPN.RPN] -> Output -> ShuntingYard Output
addToOutput True rpnM (expr, cast) = do
  rpn <- sequence rpnM
  return (expr, Just $ maybe rpn (rpn ++) cast)
addToOutput False rpnM (expr, cast) = do
  rpn <- sequence rpnM
  return (rpn ++ expr, cast)

addToOutputM :: Bool -> ShuntingYard [RPN.RPN] -> ShuntingYard Output -> ShuntingYard Output
addToOutputM cast rpnsM outputM = do
  rpns <- rpnsM
  output <- outputM
  addToOutput cast (map return rpns) output

operatorToFun :: E.Position -> String -> ShuntingYard RPN.RPN
operatorToFun pos name = do
  operator <- C.getOperator name
  case operator of
    Nothing -> C.throwString pos "unknown operator"
    Just operator -> return $ RPN.Function pos name $ C._function operator

-- the boolean is used for negation
shuntingYard' :: Bool -> [T.Token] -> ShuntingYard Output
-- end of algorithm, everything thats still on the operator stack is pushed to output
shuntingYard' _ [] = C.get >>= popWholeStack . _stack
shuntingYard' start (t : ts) = do
  cast <- _isCast <$> C.get
  case t of
    T.Token (T.Value r)           pos -> handleValue r pos ts
    T.Token (T.Unit u r symbol)   pos -> shuntingYard' False ts >>= addToOutput cast [return $ RPN.Value pos $ V.Value r u symbol]
    T.Token  T.OpeningBracket     pos -> C.modify (stack %:~ OpeningBracket pos '(') >> shuntingYard' False ts
    T.Token  T.ClosingBracket     pos -> addToOutputM cast (popUntilBracket '(' pos) (shuntingYard' False ts)
    T.Token (T.Operator operator) pos -> handleOperator start operator pos ts
    T.Token  T.CastStart          pos -> C.modify ((isCast .~ True) . (stack %:~ OpeningBracket pos '[')) >> shuntingYard' False ts
    T.Token  T.CastEnd            pos -> handleCastEnd cast pos ts
    T.Token (T.Variable var)      pos -> C.modify (stack %:~ Function pos var (throwE "temp" :: Except String V.Value)) >> shuntingYard' False ts


handleCastEnd :: Bool -> E.Position -> [T.Token] -> ShuntingYard Output
handleCastEnd cast pos [] = addToOutputM cast (popUntilBracket '[' pos) (shuntingYard' False []) <* C.modify (isCast .~ False)
handleCastEnd _    pos _  = C.throwString pos "a cast has to be the last thing in the expression"

handleValue :: Rational -> E.Position -> [T.Token] -> ShuntingYard Output
handleValue r pos ts = do
  state <- C.get
  when (_isCast state && r /= 1) $
    C.throwString pos "only number 1 is allowed in cast"
  shuntingYard' False ts >>= addToOutput (_isCast state) [return $ RPN.Value pos $ V.fromRational r]

handleOperator :: Bool -> String -> E.Position -> [T.Token] -> ShuntingYard Output
handleOperator start name pos ts = do
  state <- C.get
  when (_isCast state && name `notElem` ["*", "/"]) $
    C.throwString pos "this operator is not allowed in cast"
  let isMinus = name == "-"
      stackHead = state ^? stack . _head . _OpeningBracket
  if isMinus && (start || isJust stackHead)
    then C.modify (stack %:~ Function pos "negate" V.vNegate) >> shuntingYard' False ts
    else addToOutputM (_isCast state) (popOperators pos name) (shuntingYard' False ts)


popWholeStack :: [OperatorStackType] -> ShuntingYard Output
popWholeStack [] = return ([], Nothing)
popWholeStack (OpeningBracket pos _ : _) = C.throwString pos "missing closing bracket"
popWholeStack (Operator pos name : rest) = popWholeStack rest >>= addToOutput False [operatorToFun pos name]
popWholeStack (Function pos name f : rest) = popWholeStack rest >>= addToOutput False [return $ RPN.Function pos name f]


popOperators :: E.Position -> String -> ShuntingYard [RPN.RPN]
popOperators pos operator = C.get >>= go . _stack
  where
    go :: [OperatorStackType] -> ShuntingYard [RPN.RPN]
    go [] = C.modify (stack .~ [Operator pos operator]) >> return []
    go s@(OpeningBracket _ _ : _) = C.modify (stack .~ Operator pos operator : s) >> return []
    go       (Function pos name f : rest) = go rest <&> (RPN.Function pos name f :)
    go s@(Operator pos name : rest) = do
      p1 <- C.getPrecedence name
      p2 <- C.getPrecedence operator
      if p1 < p2
        then C.modify (stack .~ Operator pos name : s) >> return []
        else do
          rpnFun <- operatorToFun pos name
          go rest <&> (rpnFun :)


popUntilBracket :: Char -> E.Position -> ShuntingYard [RPN.RPN]
popUntilBracket bracket pos = C.get >>= go [] . _stack
  where
    go :: [RPN.RPN] -> [OperatorStackType] -> ShuntingYard [RPN.RPN]
    go _ [] = C.throwString pos "missing opening bracket"
    go output (Operator pos name : rest) = do
      rpnFun <- operatorToFun pos name
      go (rpnFun : output) rest
    go output (Function pos name f : rest) = go (RPN.Function pos name f : output) rest
    go output (OpeningBracket pos b : rest) = if bracket == b
      then C.modify (stack .~ rest) >> return output
      else C.throwString pos "missmatching brackets"



shuntingYard :: [T.Token] -> C.Calculator a Output
shuntingYard tokens = C.replaceState (shuntingYard' True tokens) defaultState
