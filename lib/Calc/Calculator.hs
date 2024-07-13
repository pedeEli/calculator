{-# LANGUAGE TemplateHaskell, FlexibleContexts, ConstraintKinds, MultiParamTypeClasses, FlexibleInstances,
    ScopedTypeVariables, StandaloneKindSignatures, TypeApplications, TupleSections, GADTs, DataKinds, TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
module Calc.Calculator where

{-
import Control.Lens
import Control.Monad.Trans.Except
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad

import qualified Data.Map as M
import Data.Kind
import Data.Maybe

import qualified Calc.Error as E
import qualified Calc.Value as V

import Debug.Trace

type Calculator s = ExceptT E.Error (S.StateT (CalcState s) IO)


data Nat = Zero | Succ Nat


type CountArgs :: Type -> Nat
type family CountArgs f where
  CountArgs (V.Value -> res) = 'Succ (CountArgs res)
  CountArgs (Calculator () V.Value) = 'Zero


type FunctionWitness :: Type -> Nat -> Type
data FunctionWitness f n where
  FunctionFun :: Fun res => FunctionWitness (V.Value -> res) ('Succ n)
  FunctionNil :: FunctionWitness (Calculator () V.Value) 'Zero


type Function :: Type -> Nat -> Constraint
class Function f n where
  witness :: FunctionWitness f n
type Fun f = Function f (CountArgs f)


instance (Function res n, CountArgs res ~ n) => Function (V.Value -> res) ('Succ n) where
  witness = FunctionFun
instance Function (Calculator () V.Value) 'Zero where
  witness = FunctionNil


data Toplevel =
  Expression Expr |
  Definition String [String] Expr
  deriving (Show)
data Expr =
  Literal V.Value |
  Cast Expr Expr |
  Variable String |
  Application Expr Expr
  deriving (Show)

data Variable =
  Value E.Position (Calculator () Expr) |
  Lambda E.Position String Variable
instance E.WithPos Variable where
  getPosition (Value pos _) = pos
  getPosition (Lambda pos _ _) = pos



type Clousure = M.Map String (String, Variable)

data CalcState s = CalcState {
  _state :: s,
  _source :: String,
  _precedences :: M.Map String Int,
  _clousure :: Clousure,
  _clousures :: [Clousure]}
$(makeLenses 'CalcState)


mapOperator :: (V.Value -> V.Value -> Except String V.Value) -> (String, Variable)
mapOperator operator = ("", res)
  where
    res :: Variable
    res = Lambda mempty "a" $ Lambda mempty "b" $ Value mempty $ do
      (_, aResult) <- getVariable mempty "a"
      (_, bResult) <- getVariable mempty "b"
      (_, aValue) <- variableToValue aResult
      (_, bValue) <- variableToValue bResult
      let result = runExcept $ operator aValue bValue
      case result of
        Left err -> throwString mempty err
        Right value -> return $ Literal value

buildInOperators :: Clousure
buildInOperators = M.fromList [
  ("+", mapOperator (V.<<+>>)),
  ("-", mapOperator (V.<<->>)),
  ("*", mapOperator (V.<<*>>)),
  ("/", mapOperator (V.<</>>)),
  ("^", mapOperator (V.<<^>>))]

buildInPrecedences :: M.Map String Int
buildInPrecedences = M.fromList [
  ("+", 0),
  ("-", 0),
  ("*", 1),
  ("/", 1),
  ("^", 2)]


defaultState :: CalcState ()
defaultState = CalcState {
  _state = (),
  _source = "",
  _precedences = buildInPrecedences,
  _clousure = buildInOperators,
  _clousures = []}

runCalculator :: Calculator () a -> IO ()
runCalculator c = void $ S.evalStateT (runExceptT c) defaultState


get :: Calculator s s
get = _state <$> lift S.get

modify :: (s -> s) -> Calculator s ()
modify = lift . S.modify . over state

getPrecedence :: String -> Calculator s Int
getPrecedence name = do
  ps <- _precedences <$> lift S.get
  return $ fromMaybe 9 $ ps M.!? name

getVariable :: E.Position -> String -> Calculator () (String, Variable)
getVariable pos name = do
  state <- lift S.get
  case _clousure state M.!? name of
    Nothing -> throwString pos "unknown variable"
    Just variable -> return variable

addVariable :: String -> String -> Variable -> Calculator s ()
addVariable source name variable = lift $ S.modify $ clousure %~ M.insert name (source, variable)

pushClousure :: Calculator s ()
pushClousure = do
  clousure <- _clousure <$> lift S.get
  lift $ S.modify (clousures %~ (clousure :))

popClousure :: Calculator s ()
popClousure = do
  cs <- _clousures <$> lift S.get
  case cs of
    [] -> return ()
    (c : rest) -> lift $ S.modify $ (clousure .~ c) . (clousures .~ rest)

getSource :: Calculator s String
getSource = _source <$> lift S.get

putSource :: String -> Calculator s ()
putSource = lift . S.modify . set source


throwError :: E.Error -> Calculator s a
throwError = throwE

throwString :: E.Position -> String -> Calculator s a
throwString pos message = do
  source <- getSource
  throwE $ E.Error pos source $ E.Message message


replaceState :: Calculator s b -> s -> Calculator s' b
replaceState calc s = do
  s' <- lift S.get
  (result, s) <- liftIO $ S.runStateT (runExceptT calc) (s' & state .~ s)
  case result of
    Left err -> throwError err
    Right b -> do
      lift $ S.put (s & state .~ _state s')
      return b


try :: Calculator s a -> Calculator s (Either E.Error a)
try = tryE


evaluateFunction :: forall f. Fun f => E.Position -> [(E.Position, V.Value)] -> f -> Calculator () (E.Position, V.Value)
evaluateFunction pos values f = case witness @f @(CountArgs f) of
  FunctionNil -> if null values then (pos,) <$> f
    else throwString (mconcat $ map fst values) "to many arguments for function"
  FunctionFun -> case values of
    [] -> throwString pos "too few arguments for function"
    ((p, value) : rest) -> evaluateFunction (pos <> p) rest (f value)


evaluate :: Expr -> Calculator () (E.Position, t)
evaluate = undefined
-- evaluate (Definition pos source name ast) = do
--   addVariable source name ast
--   return (pos, ())
-- evaluate (Literal pos v) = return (pos, v)
-- evaluate (Cast pos valueAst castAst) = do
--   (valuePos, value) <- evaluate valueAst
--   (castPos, cast) <- evaluate castAst
--   when (V._vUnit value /= V._vUnit cast) $
--     throwString pos "missmatched units in cast"
--   return (valuePos <> castPos, value & V.vBase //~ V._vBase cast & V.vUnitOverride .~ V._vUnitOverride cast)
-- evaluate (Variable pos name) = do
--   (source, result) <- getVariable pos name
--   putSource source
--   return (pos, result)
-- evaluate (BuildIn c) = (mempty,) <$> c
-- evaluate (Application pos left right) = do
--   (leftPos, name, result) <- resultToLambda left
--   pushClousure
--   addVariable "" name right
  
--   popClousure
--   _


variableToValue :: Variable -> Calculator () (E.Position, V.Value)
variableToValue (Lambda pos _ _) = throwString pos "expected a value"
variableToValue (Value _ expr) = expr >>= evaluate

variableToLambda :: Variable -> Calculator () (E.Position, String, Variable)
variableToLambda (Value pos _) = throwString pos "expected a function"
variableToLambda (Lambda pos name variable) = return (pos, name, variable)

-}