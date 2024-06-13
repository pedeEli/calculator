module Calc (calc) where

import Calc.Token (tokenize)
import Calc.ShuntingYard (shuntingYard)
import Calc.RPN (evaluate)
import Calc.Value (Value(..), _vUnit, _vBase, _vUnitOverride, vBase, vUnitOverride)

import Control.Lens
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Control.Monad.IO.Class (MonadIO(liftIO))

calc :: String -> IO (Maybe Value)
calc str = runMaybeT $ do
  (tokens, cast) <- tokenize str
  -- liftIO $ print tokens
  rpn <- shuntingYard tokens
  -- liftIO $ print rpn
  value <- evaluate rpn
  case value of
    Error err -> liftIO (putStrLn err) >> fail ""
    _ -> case cast of
      Nothing -> return value
      Just cast -> do
        rpn <- shuntingYard cast
        unit <- evaluate rpn
        case unit of
          Error err -> liftIO (putStrLn err) >> fail ""
          _ -> do
            if _vUnit value /= _vUnit unit
              then return $ Error "missmatched units"
              else return $ value & vBase //~ (_vBase unit) & vUnitOverride .~ (_vUnitOverride unit)