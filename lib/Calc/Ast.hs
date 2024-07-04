{-# LANGUAGE GADTs, StandaloneDeriving #-}
module Calc.Ast where

import qualified Calc.Value as V


data Ast t where
  Variable :: String -> [Ast V.Value] -> Ast V.Value
  Value :: V.Value -> Ast V.Value
  Cast :: Ast V.Value -> Ast V.Value -> Ast V.Value
  Definition :: String -> [String] -> Ast V.Value -> Ast ()

deriving instance Show (Ast t)