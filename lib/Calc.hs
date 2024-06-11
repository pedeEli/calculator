module Calc (calc) where

import Calc.Token (tokenize)
import Calc.ShuntingYard (shuntingYard)
import Calc.RPN (evaluate)
import Calc.Value (Value)

import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Control.Monad.IO.Class (MonadIO(liftIO))

calc :: String -> IO (Maybe Value)
calc str = runMaybeT $ do
  tokens <- tokenize str
  -- liftIO $ print tokens
  rpn <- shuntingYard tokens
  -- liftIO $ print rpn
  evaluate rpn