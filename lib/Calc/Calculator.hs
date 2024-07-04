{-# LANGUAGE TemplateHaskell, FlexibleContexts, ConstraintKinds, MultiParamTypeClasses, FlexibleInstances,
    ScopedTypeVariables, StandaloneKindSignatures, TypeApplications, TupleSections, GADTs, DataKinds, TypeFamilies #-}
module Calc.Calculator where


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
import qualified Calc.Ast as Ast


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



data BuildIn = forall f. Fun f => BuildIn f

data CalcState s = CalcState {
  _state :: s,
  _precedences :: M.Map String Int,
  _buildIns :: M.Map String BuildIn,
  _asts :: M.Map String ([String], Ast.Ast V.Value)}
$(makeLenses 'CalcState)


mapOperator :: (V.Value -> V.Value -> Except String V.Value) -> V.Value -> V.Value -> Calculator () V.Value
mapOperator operator a b =
  let result = runExcept $ operator a b
  in case result of
    Left err -> throwString mempty err
    Right value -> return value

buildInOperators :: M.Map String BuildIn
buildInOperators = M.fromList [
  ("+", BuildIn $ mapOperator (V.<<+>>)),
  ("-", BuildIn $ mapOperator (V.<<->>)),
  ("*", BuildIn $ mapOperator (V.<<*>>)),
  ("/", BuildIn $ mapOperator (V.<</>>)),
  ("^", BuildIn $ mapOperator (V.<<^>>))]

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
  return $ fromMaybe 9 $ ps M.!? name

getBuildIn :: String -> Calculator () (Maybe BuildIn)
getBuildIn name = do
  state <- lift S.get
  return $ _buildIns state M.!? name

getVariable :: String -> Calculator () (Maybe ([String], Ast.Ast V.Value))
getVariable name = do
  state <- lift S.get
  return $ _asts state M.!? name


addAst :: String -> [String] -> Ast.Ast V.Value -> Calculator s ()
addAst name args ast = lift $ S.modify $ asts %~ M.insert name (args, ast)

getAsts :: Calculator s (M.Map String ([String], Ast.Ast V.Value))
getAsts = _asts <$> lift S.get

putAsts :: M.Map String ([String], Ast.Ast V.Value) -> Calculator s ()
putAsts = lift . S.modify . set asts


throwError :: E.Error -> Calculator s a
throwError = throwE

throwString :: E.Position -> String -> Calculator s a
throwString pos = throwE . E.Error pos . E.Message


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


evaluateFunction :: forall f. Fun f => E.Position -> [(E.Position, V.Value)] -> f -> Calculator () [(E.Position, V.Value)]
evaluateFunction pos ds f = case witness @f @(CountArgs f) of
  FunctionNil -> (:[]) . (pos,) <$> f
  FunctionFun -> case ds of
    [] -> throwString pos "too few arguments to function"
    ((p, d) : rest) -> evaluateFunction (pos <> p) rest $ f d

evaluateFunction' :: forall f. Fun f => [V.Value] -> f -> Calculator () V.Value
evaluateFunction' values f = case witness @f @(CountArgs f) of
  FunctionNil -> if null values then f
    else throwString mempty "to many arguments for function"
  FunctionFun -> case values of
    [] -> throwString mempty "too few arguments for function"
    (value : rest) -> evaluateFunction' rest (f value)


evaluate :: Ast.Ast t -> Calculator () t
evaluate (Ast.Value v) = return v
evaluate (Ast.Definition name args ast) = addAst name args ast
evaluate (Ast.Cast valueAst castAst) = do
  value <- evaluate valueAst
  cast <- evaluate castAst
  when (V._vUnit value /= V._vUnit cast) $
    throwString mempty "missmatched units in cast"
  return $ value & V.vBase //~ V._vBase cast & V.vUnitOverride .~ V._vUnitOverride cast
evaluate (Ast.Variable name args) = do
  buildIn <- getBuildIn name
  case buildIn of
    Just (BuildIn f) -> do
      args' <- mapM evaluate args
      evaluateFunction' args' f
    Nothing -> do
      variable <- getVariable name
      case variable of
        Nothing -> throwString mempty "unknown variable"
        Just (argNames, ast) -> do
          backup <- getAsts
          setArgs argNames args
          result <- evaluate ast
          putAsts backup
          return result
  where
    setArgs :: [String] -> [Ast.Ast V.Value] -> Calculator () ()
    setArgs [] [] = return ()
    setArgs [] _ = throwString mempty "too many argumets for functions"
    setArgs _ [] = throwString mempty "too few argumets for functions"
    setArgs (name : names) (value : values) = do
      addAst name [] value
      setArgs names values