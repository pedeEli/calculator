module Calc (calc) where

import Calc.Token (tokenize, Token)
import Calc.ShuntingYard (shuntingYard)
import Calc.RPN (evaluate)
import Calc.Value as V (Value(..), _vUnit, _vBase, _vUnitOverride, vBase, vUnitOverride)
import Calc.Error as E (Error(..), ErrorMessage(..))

import Control.Lens
import Control.Monad.Trans.Except (Except, tryE, runExcept, throwE)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad (when)

-- import Debug.Trace

calc :: String -> Either Error Value
calc str = runExcept $ do
  (tokens, cast) <- tokenize str
  -- traceM $ show tokens
  rpn <- shuntingYard tokens
  -- traceM $ show rpn
  value <- evaluate rpn
  applyCast value cast


applyCast :: Value -> [Token] -> Except Error Value
applyCast v [] = return v
applyCast v cast = do
  rpn <- shuntingYard cast
  unit <- evaluate rpn
  when (_vUnit v /= _vUnit unit) $ throwE $ Error mempty $ Message "missmatched units in cast"
  return $ v & vBase //~ _vBase unit & vUnitOverride .~ _vUnitOverride unit