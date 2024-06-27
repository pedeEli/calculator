module Calc (calc) where

import Calc.Token (tokenize, Token)
import Calc.ShuntingYard (shuntingYard, Output(..))
import Calc.RPN (evaluate)
import Calc.Value as V
import Calc.Error as E (Error(..), ErrorMessage(..))

import Control.Lens
import Control.Monad.Trans.Except (Except, tryE, runExcept, throwE)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad (when)

-- import Debug.Trace

calc :: String -> Either Error Value
calc str = runExcept $ do
  tokens <- tokenize str
  -- traceM $ show tokens
  rpn <- shuntingYard tokens
  -- traceM $ show rpn
  case rpn of
    Definition _ _ -> throwE $ Error mempty $ Message "definitions are not yet supported"
    Expression expr cast -> do    
      value <- stripUnitOverride <$> evaluate expr
      case cast of
        Nothing -> return value
        Just rpnCast -> do
          unit <- evaluate rpnCast
          when (_vUnit value /= _vUnit unit) $ throwE $ Error mempty $ Message "missmatched units in cast"
          return $ value & vBase //~ _vBase unit & vUnitOverride .~ _vUnitOverride unit