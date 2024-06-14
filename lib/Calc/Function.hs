{-# LANGUAGE StandaloneKindSignatures, DataKinds, TypeFamilies, GADTs,
    ScopedTypeVariables, TypeApplications, FlexibleContexts, ConstraintKinds,
    MultiParamTypeClasses, FlexibleInstances #-}
module Calc.Function where

import Control.Monad.Trans.Except (Except, throwE)

import Data.Kind (Type, Constraint)

import Calc.Value (Value)
import Calc.Error (Position, Error(..), ErrorMessage(..))


data Nat = Zero | Succ Nat


type CountArgs :: Type -> Nat
type family CountArgs f where
  CountArgs (Value -> res) = 'Succ (CountArgs res)
  CountArgs other = 'Zero


type FunctionWitness :: Type -> Nat -> Type
data FunctionWitness f n where
  FunctionFun :: Fun res => FunctionWitness (Value -> res) ('Succ n)
  FunctionNil :: FunctionWitness Value 'Zero


type Function :: Type -> Nat -> Constraint
class Function f n where
  witness :: FunctionWitness f n
type Fun f = Function f (CountArgs f)


instance (Function res n, CountArgs res ~ n) => Function (Value -> res) ('Succ n) where
  witness = FunctionFun
instance Function Value 'Zero where
  witness = FunctionNil



evaluateFunction :: forall f. Fun f => Position -> [(Position, Value)] -> f -> Except Error [(Position, Value)]
evaluateFunction pos ds f = case witness @f @(CountArgs f) of
  FunctionNil -> return $ (pos, f) : ds
  FunctionFun -> case ds of
    [] -> throwE $ Error pos $ Message "too few arguments to function"
    ((p, d) : rest) -> evaluateFunction (pos <> p) rest $ f d