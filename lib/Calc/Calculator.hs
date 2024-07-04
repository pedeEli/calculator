{-# LANGUAGE TemplateHaskell, FlexibleContexts, ConstraintKinds, MultiParamTypeClasses, FlexibleInstances,
    ScopedTypeVariables, StandaloneKindSignatures, TypeApplications, TupleSections, GADTs, DataKinds, TypeFamilies #-}
module Calc.Calculator where


import Control.Lens
import Control.Monad.Trans.Except
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad

import Data.Map as M
import Data.Kind
import Data.Type.Equality
import Data.Proxy

import Calc.Error
import Calc.Value
import Data.Maybe (fromMaybe)


type Calculator s a = ExceptT Error (S.StateT (CalcState s) IO) a

type ASTOperator = Value -> Value -> Calculator () Value

data Ast t where
  Function :: String -> [Ast Value] -> Ast Value
  Value    :: Value -> Ast Value



data Nat = Zero | Succ Nat


type CountArgs :: Type -> Nat
type family CountArgs f where
  CountArgs (Value -> res) = 'Succ (CountArgs res)
  CountArgs (Calculator () Value) = 'Zero


type FunctionWitness :: Type -> Nat -> Type
data FunctionWitness f n where
  FunctionFun :: Fun res => FunctionWitness (Value -> res) ('Succ n)
  FunctionNil :: FunctionWitness (Calculator () Value) 'Zero


type Function :: Type -> Nat -> Constraint
class Function f n where
  witness :: FunctionWitness f n
type Fun f = Function f (CountArgs f)


instance (Function res n, CountArgs res ~ n) => Function (Value -> res) ('Succ n) where
  witness = FunctionFun
instance Function (Calculator () Value) 'Zero where
  witness = FunctionNil


evaluateFunction :: forall f. Fun f => Position -> [(Position, Value)] -> f -> Calculator () [(Position, Value)]
evaluateFunction pos ds f = case witness @f @(CountArgs f) of
  FunctionNil -> (:[]) . (pos,) <$> f
  FunctionFun -> case ds of
    [] -> throwE $ Error pos $ Message "too few arguments to function"
    ((p, d) : rest) -> evaluateFunction (pos <> p) rest $ f d

data BuildIn = forall f. Fun f => BuildIn f

data CalcState s = CalcState {
  _state :: s,
  _precedences :: Map String Int,
  _buildIns :: Map String BuildIn,
  _asts :: Map String (Ast Value)}
$(makeLenses 'CalcState)


mapOperator :: (Value -> Value -> Except String Value) -> Value -> Value -> Calculator () Value
mapOperator operator a b =
  let result = runExcept $ operator a b
  in case result of
    Left err -> throwString mempty err
    Right value -> return value

buildInOperators :: Map String BuildIn
buildInOperators = fromList [
  ("+", BuildIn $ mapOperator (<<+>>)),
  ("-", BuildIn $ mapOperator (<<->>)),
  ("*", BuildIn $ mapOperator (<<*>>)),
  ("/", BuildIn $ mapOperator (<</>>)),
  ("^", BuildIn $ mapOperator (<<^>>))]

buildInPrecedences :: Map String Int
buildInPrecedences = fromList [
  ("+", 0),
  ("-", 0),
  ("*", 1),
  ("/", 1),
  ("^", 2)]


defaultState :: CalcState ()
defaultState = CalcState {
  _state = (),
  _precedences = buildInPrecedences,
  _buildIns = buildInOperators,
  _asts = mempty}

runCalculator :: Calculator () a -> IO ()
runCalculator c = void $ S.evalStateT (runExceptT c) defaultState


get :: Calculator s s
get = _state <$> lift S.get

modify :: (s -> s) -> Calculator s ()
modify = lift . S.modify . over state

getPrecedence :: String -> Calculator s Int
getPrecedence name = do
  ps <- _precedences <$> lift S.get
  return $ fromMaybe 9 $ ps !? name


addAst :: String -> Ast Value -> Calculator s ()
addAst name ast = lift $ S.modify $ asts %~ insert name ast

getAsts :: Calculator s (Map String (Ast Value))
getAsts = _asts <$> lift S.get

putAsts :: Map String (Ast Value) -> Calculator s ()
putAsts = lift . S.modify . set asts


throwError :: Error -> Calculator s a
throwError = throwE

throwString :: Position -> String -> Calculator s a
throwString pos = throwE . Error pos . Message


replaceState :: Calculator s b -> s -> Calculator s' b
replaceState calc s = do
  s' <- lift S.get
  (result, s) <- liftIO $ S.runStateT (runExceptT calc) (s' & state .~ s)
  case result of
    Left err -> throwError err
    Right b -> do
      lift $ S.put (s & state .~ _state s')
      return b


try :: Calculator s a -> Calculator s (Either Error a)
try = tryE