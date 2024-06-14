module Calc (calc) where

import Calc.Token (tokenize, Token)
import Calc.ShuntingYard (shuntingYard)
import Calc.RPN (evaluate)
import Calc.Value as V (Value(..), _vUnit, _vBase, _vUnitOverride, vBase, vUnitOverride)
import Calc.Error as E (Error(..), ErrorMessage(..))

import Control.Lens
import Control.Monad.Trans.Except (Except, tryE, runExcept, throwE)
import Control.Monad.IO.Class (MonadIO(liftIO))

calc :: String -> Either Error Value
calc str = runExcept $ do
  (tokens, cast) <- tokenize str
  -- liftIO $ print tokens
  rpn <- shuntingYard tokens
  -- liftIO $ print rpn
  value <- evaluate rpn
  case value of
    V.Error err -> throwE $ E.Error mempty $ Message err
    _ -> applyCast value cast


applyCast :: Value -> [Token] -> Except Error Value
applyCast v [] = return v
applyCast v cast = do
  rpn <- shuntingYard cast
  unit <- evaluate rpn
  case unit of
    V.Error err -> throwE $ E.Error mempty $ Message err
    _ -> return $ if _vUnit v /= _vUnit unit
      then V.Error "missmatched units"
      else v & vBase //~ _vBase unit & vUnitOverride .~ _vUnitOverride unit