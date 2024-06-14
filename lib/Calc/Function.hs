{-# LANGUAGE StandaloneKindSignatures, DataKinds, TypeFamilies, GADTs,
    ScopedTypeVariables, TypeApplications, FlexibleContexts, ConstraintKinds,
    MultiParamTypeClasses, FlexibleInstances #-}
module Calc.Function where


import Data.Kind (Type, Constraint)

import Calc.Value (Value)


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



evaluateFunction :: forall f. Fun f => [Value] -> f -> [Value]
evaluateFunction ds f = case witness @f @(CountArgs f) of
  FunctionNil -> f : ds
  FunctionFun -> case ds of
    [] -> error "to few arguments"
    (d : rest) -> evaluateFunction rest $ f d