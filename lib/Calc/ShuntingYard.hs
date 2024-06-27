{-# LANGUAGE TemplateHaskell, ExistentialQuantification, FlexibleContexts #-}
module Calc.ShuntingYard where

import Data.List (find)
import Data.Maybe (isJust)

import Control.Lens
import Control.Monad (when)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State (StateT, evalStateT, put, get, modify)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.IO.Class (MonadIO(liftIO))

import Calc.Token (Token(..), TokenType(..))
import Calc.RPN (RPN(..))
import Calc.Value as V
import Calc.Function (Fun)
import Calc.Error (Error(..), Position, ErrorMessage(..))

import Debug.Trace

data OperatorInfo = OperatorInfo {
  _opType :: String,
  _opPrecedence :: Word,
  _fun :: Value -> Value -> Except String Value}

$(makeLenses 'OperatorInfo)

buildInOperators :: [OperatorInfo]
buildInOperators = [
  OperatorInfo "+" 0 (flip (<<+>>)),
  OperatorInfo "-" 0 (flip (<<->>)),
  OperatorInfo "*" 1 (flip (<<*>>)),
  OperatorInfo "/" 1 (flip (<</>>)),
  OperatorInfo "^" 2 (flip (<<^>>))]

data OperatorStackType =
  Operator Position OperatorInfo |
  forall f. Fun f => Function Position String f |
  OpeningBracket Position Char
instance Show OperatorStackType where
  show (Operator _ info) = "Operator " ++ _opType info
  show (Function _ name _) = "Function " ++ name
  show (OpeningBracket _ b) = "OpeningBracket " ++ [b]

$(makePrisms ''OperatorStackType)

data Output =
  Expression [RPN] (Maybe [RPN]) |
  Definition [RPN] [RPN]
  deriving (Show)

data State = State {_stack :: [OperatorStackType], _isCast :: Bool}
  deriving (Show)
$(makeLenses 'State)

defaultState :: State
defaultState = State {_stack = [], _isCast = False}

type ShuntingYard a = StateT State (Except Error) a


addToOutput :: Bool -> [RPN] -> Output -> Output
addToOutput _     rpn (Definition left right) = Definition left (rpn ++ right)
addToOutput True  rpn (Expression expr cast)  = Expression expr $ Just $ maybe rpn (rpn ++) cast
addToOutput False rpn (Expression expr cast)  = Expression (rpn ++ expr) cast

addToOutputM :: Bool -> ShuntingYard [RPN] -> ShuntingYard Output -> ShuntingYard Output
addToOutputM cast rpnsM outputM = do
  rpns <- rpnsM
  output <- outputM
  return $ addToOutput cast rpns output

operatorToFun :: Position -> OperatorInfo -> RPN
operatorToFun pos info = RPN'Function pos (_opType info) (_fun info)

-- the boolean is used for negation
shuntingYard' :: Bool -> [Token] -> ShuntingYard Output
shuntingYard' _ [] = get >>= shuntingYardEnd . _stack
shuntingYard' start (t : ts) = do
  cast <- _isCast <$> get
  case t of
    Token (Token'Value r)           pos -> handleValue r pos ts
    Token (Token'Unit u r symbol)   pos -> shuntingYard' False ts <&> addToOutput cast [RPN'Value pos $ Value r u symbol]
    Token Token'OpeningBracket      pos -> modify (stack %~ (OpeningBracket pos '(' :)) >> shuntingYard' False ts
    Token Token'ClosingBracket      pos -> addToOutputM cast (popUntilBracket '(' pos) (shuntingYard' False ts)
    Token (Token'Operator operator) pos -> handleOperator start operator pos ts
    Token Token'CastStart           pos -> modify ((isCast .~ True) . (stack %~ (OpeningBracket pos '[' :))) >> shuntingYard' False ts
    Token Token'CastEnd             pos -> if null ts
      then addToOutputM cast (popUntilBracket '[' pos) (shuntingYard' False ts) <* modify (isCast .~ False)
      else lift $ throwE $ Error pos $ Message "a cast has to be the last thing in the expression"


handleValue :: Rational -> Position -> [Token] -> ShuntingYard Output
handleValue r pos ts = do
  state <- get
  when (_isCast state && r /= 1) $
    lift $ throwE $ Error pos $ Message "only number 1 is allowed in cast"
  shuntingYard' False ts <&> addToOutput (_isCast state) [RPN'Value pos $ V.fromRational r]

handleOperator :: Bool -> String -> Position -> [Token] -> ShuntingYard Output
handleOperator start operator pos ts = case find (\a -> operator == _opType a) buildInOperators of
  Nothing -> lift $ throwE $ Error pos $ Message "unknown operator"
  Just info -> do
    state <- get
    when (_isCast state && operator `notElem` ["*", "/"]) $
      lift $ throwE $ Error pos $ Message "this operator is not allowed in cast"
    let isMinus = operator == "-"
        stackHead = state ^? stack . _head . _OpeningBracket
    if isMinus && (start || isJust stackHead)
      then modify (stack %~ (Function pos "negate" vNegate :)) >> shuntingYard' False ts
      else addToOutputM (_isCast state) (popOperators pos info) (shuntingYard' False ts)

-- end of algorithm, everything thats still on the operator stack is pushed to output
shuntingYardEnd :: [OperatorStackType] -> ShuntingYard Output
shuntingYardEnd [] = return $ Expression [] Nothing
shuntingYardEnd (OpeningBracket pos _ : _) = lift $ throwE $ Error pos $ Message "missing closing bracket"
shuntingYardEnd (Operator pos info : rest) = shuntingYardEnd rest <&> addToOutput False [operatorToFun pos info]
shuntingYardEnd (Function pos name f : rest) = shuntingYardEnd rest <&> addToOutput False [RPN'Function pos name f]


popOperators :: Position -> OperatorInfo -> ShuntingYard [RPN]
popOperators pos info = get >>= go . _stack
  where
    go :: [OperatorStackType] -> ShuntingYard [RPN]
    go [] = modify (stack .~ [Operator pos info]) >> return []
    go s@(OpeningBracket _ _ : _) = modify (stack .~ Operator pos info : s) >> return []
    go       (Function pos name f : rest) = go rest <&> (RPN'Function pos name f :)
    go s@(Operator pos i : rest) = if _opPrecedence i < _opPrecedence info
      then modify (stack .~ Operator pos info : s) >> return []
      else go rest <&> (RPN'Function pos (_opType i) (_fun i) :)


popUntilBracket :: Char -> Position -> ShuntingYard [RPN]
popUntilBracket bracket pos = get >>= go [] . _stack
  where
    go :: [RPN] -> [OperatorStackType] -> ShuntingYard [RPN]
    go _ [] = lift $ throwE $ Error pos $ Message "missing opening bracket"
    go output (Operator pos info : rest) = go (operatorToFun pos info : output) rest
    go output (Function pos name f : rest) = go (RPN'Function pos name f : output) rest
    go output (OpeningBracket pos b : rest) = if bracket == b
      then modify (stack .~ rest) >> return output
      else lift $ throwE $ Error pos $ Message "missmatching brackets"



shuntingYard :: [Token] -> Except Error Output
shuntingYard tokens = evalStateT (shuntingYard' True tokens) defaultState
