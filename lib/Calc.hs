module Calc (calc) where

import Calc.Token (tokenize, Cast(..))
import Calc.ShuntingYard (shuntingYard)
import Calc.RPN (evaluate)
import Calc.Value (Value, _vUnit)

import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Control.Monad.IO.Class (MonadIO(liftIO))

calc :: String -> IO (Maybe Value)
calc str = runMaybeT $ do
  (tokens, cast) <- tokenize str
  -- liftIO $ print tokens
  rpn <- shuntingYard tokens
  -- liftIO $ print rpn
  value <- evaluate rpn
  case cast of
    Nothing -> return value
    Just (Cast tokens symbol) -> do
      rpn <- shuntingYard tokens
      cast <- evaluate rpn
      if _vUnit value == _vUnit cast
        then return (value / cast)
        else liftIO (putStrLn "cast failed: missmatched units") >> fail ""