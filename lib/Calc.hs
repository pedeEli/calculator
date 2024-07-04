{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, DataKinds, KindSignatures, ExistentialQuantification, TypeApplications #-}
module Calc (calc, C.Calculator, C.runCalculator, liftIO) where

import qualified Calc.RPN as RPN
import qualified Calc.Value as V
import qualified Calc.Error as E
import qualified Calc.Token as T
import qualified Calc.Calculator as C
import qualified Calc.ShuntingYard as SY

import Control.Lens
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad

-- import Debug.Trace

calc :: String -> C.Calculator () (Either E.Error V.Value)
calc str = C.try $ do
  output <- T.tokenize str
  handleTokenOutput output



handleTokenOutput :: T.Output -> C.Calculator () V.Value
handleTokenOutput (T.Definition leftSide rightSide) = do
  left <- parseLeftSide leftSide
  case left of
    Operator name aName bName -> do
      C.addOperator name $ C.Operator {C._precedence = 9, C._function = \aVal bVal -> do
        C.addVariable aName $ C.Variable {C._variable = return aVal :: C.Calculator () V.Value}
        C.addVariable bName $ C.Variable {C._variable = return bVal :: C.Calculator () V.Value}
        handleTokenOutput $ T.Expression rightSide
        }
      return $ V.fromRational 0
    Function name vars -> do
      C.addVariable name $ C.Variable {C._variable = C.test vars []}
      return $ V.fromRational 0
      where
        -- go :: (C.Fun f) => vars -> [C.Calculator () ()] -> f
        -- go = undefined
        -- go [] acc = do
        --   sequence_ acc
        --   handleTokenOutput $ T.Expression rightSide
        -- go (name : names) acc = \val -> go names (C.addVariable name C.Variable {C._variable = return val :: C.Calculator () V.Value} : acc)

handleTokenOutput (T.Expression expr) = do
  -- traceM $ show expr
  (rpn, cast) <- SY.shuntingYard expr
  -- traceM $ show rpn
  -- traceM $ show cast
  value <- V.stripUnitOverride <$> RPN.evaluate rpn
  applyCast value cast
  



applyCast :: V.Value -> Maybe [RPN.RPN] -> C.Calculator () V.Value
applyCast value Nothing = return value
applyCast value (Just rpnCast) = do
  unit <- RPN.evaluate rpnCast
  when (V._vUnit value /= V._vUnit unit) $
    C.throwString (RPN.rpnToPosition rpnCast) "missmatched units in cast"
  return $ value & V.vBase //~ V._vBase unit & V.vUnitOverride .~ V._vUnitOverride unit




data DefinitionType =
  Operator String String String |
  Function String [String]


parseLeftSide :: [T.Token] -> C.Calculator () DefinitionType
parseLeftSide [
  T.Token (T.Variable a) _,
  T.Token (T.Operator operator) _,
  T.Token (T.Variable b) _] = return $ Operator operator a b
parseLeftSide (T.Token (T.Variable name) _ : rest) = do
  vars <- mapM go rest
  return $ Function name vars
  where
    go :: T.Token -> C.Calculator () String
    go (T.Token (T.Variable name) _) = return name
    go (T.Token _ pos) = C.throwString pos "not allowed on left side"
parseLeftSide (T.Token _ pos : _) = C.throwString pos "not allowed on left side"