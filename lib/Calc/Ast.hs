{-# LANGUAGE GADTs, StandaloneDeriving #-}
module Calc.Ast where

import qualified Calc.Value as V
import qualified Calc.Error as E


data Ast t where
  Variable :: E.Position -> String -> [Ast V.Value] -> Ast V.Value
  Value :: E.Position -> V.Value -> Ast V.Value
  Cast :: E.Position -> Ast V.Value -> Ast V.Value -> Ast V.Value
  Definition :: E.Position -> String -> String -> [String] -> Ast V.Value -> Ast ()

deriving instance Show (Ast t)