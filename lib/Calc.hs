{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, DataKinds, KindSignatures, ExistentialQuantification, TypeApplications #-}
module Calc (calc, C.Calculator, C.runCalculator, liftIO) where

import qualified Calc.Value as V
import qualified Calc.Error as E
import qualified Calc.Token as T
import qualified Calc.Calculator as C

import Control.Lens
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad

import Debug.Trace

calc :: String -> C.Calculator () (Either E.Error V.Value)
calc str = C.try $ do
  C.putSource str
  output <- C.try $ T.expression str
  case output of
    Right value -> snd <$> C.evaluate value
    Left _ -> do
      output <- T.definition str
      C.evaluate (output str)
      return $ V.fromRational 0