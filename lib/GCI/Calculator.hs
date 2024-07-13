{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, GADTs, FlexibleInstances #-}
module GCI.Calculator where


import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad

import Text.Parsec
import Text.Parsec.Error

import Data.Map as M
import Data.Word

import Language.Calc.Syntax.Expr
import Language.Calc.Syntax.Decl
import Language.Calc.Syntax.Lit

import GCI.Calc.Extension

import GCI.Parser.Expr
import GCI.Parser.Decl

import GCI.Renamer.Types as Rn
import GCI.Renamer.Expr
import GCI.Renamer.Decl

import GCI.Typechecker.Expr
import GCI.Typechecker.Decl

import GCI.Types.Names
import GCI.Types.SrcLoc
import GCI.Types.Value as V

import GCI.Core.Expr


type Calculator = StateT CState IO
type ECalculator = ExceptT (Located String) Calculator
data CState = CState {
  rn_state :: RnState,
  variables :: Map Unique Expr,
  build_ins :: Map Unique (ECalculator Value),
  strip_override :: Bool}
  deriving (Show)

instance {-# OVERLAPS #-} Show (ECalculator Value) where
  show _ = "ECalculator Value"


getVariable :: Unique -> ECalculator Expr
getVariable uname = do
  s <- lift get
  let vars = variables s
      Just exp = M.lookup uname vars
  return exp

setVariable :: Unique -> Expr -> Calculator ()
setVariable uname exp = modify $ \s -> s {
  variables = M.insert uname exp $ variables s}

startCalculator :: Calculator a -> IO a
startCalculator calc = evalStateT calc $ CState {
  strip_override = True,
  rn_state = defaultState,
  variables = mempty,
  build_ins = mempty}


addBuildIn1 :: String -> (Value -> ExceptT (Located String) Calculator Value) -> Calculator ()
addBuildIn1 name f = do
  Right uname <- runExceptT $ runRn $ mkUniqueName name
  Right arg1 <- runExceptT $ runRn $ mkUniqueName "a"
  let g = do
        a_exp <- getVariable arg1
        ValR a_val <- evaluate a_exp
        f a_val
      exp = Lam arg1 $ BuildIn uname
      ty = Lambda Rn.Value Rn.Value
  runExceptT $ runRn $ do
    addType uname ty
    addVariable name uname
  setVariable uname exp
  modify $ \s -> s {build_ins = M.insert uname g $ build_ins s}

addBuildIn2 :: String -> Int -> (Value -> Value -> ExceptT (Located String) Calculator Value) -> Calculator ()
addBuildIn2 name fix f = do
  Right uname <- runExceptT $ runRn $ mkUniqueName name
  Right arg1 <- runExceptT $ runRn $ mkUniqueName "a"
  Right arg2 <- runExceptT $ runRn $ mkUniqueName "b"
  let g = do
        a_exp <- getVariable arg1
        b_exp <- getVariable arg2
        ValR a_val <- evaluate a_exp
        ValR b_val <- evaluate b_exp
        f b_val a_val
      exp = Lam arg1 $ Lam arg2 $ BuildIn uname
      ty = Lambda Rn.Value $ Lambda Rn.Value Rn.Value
  runExceptT $ runRn $ do
    addType uname ty
    addVariable name uname
    setFixity uname $ Fixity fix
  setVariable uname exp
  modify $ \s -> s {build_ins = M.insert uname g $ build_ins s}

convertOperator :: Word64 -> (Value -> Value -> ECalculator Value) -> ECalculator Value
convertOperator i op = do
  a_expr <- getVariable $ Unique "a" i
  b_expr <- getVariable $ Unique "b" i
  a_result <- evaluate a_expr
  b_result <- evaluate b_expr
  case (a_result, b_result) of
    (ValR a_value, ValR b_value) -> op b_value a_value
    _ -> throwE $ L mempty "cannot apply lambda to build in operator"

data Interpret =
  ValI String |
  ErrorI (Located String) |
  Success

interpret :: String -> Calculator Interpret
interpret str = do
  let decl_ps = runParser parseDeclaration () "<interactive>" str
  case decl_ps of
    Right decl_ps -> interpretDecl decl_ps
    Left _ -> do
      let expr_ps = runParser parseExpression () "<interactive>" str
      case expr_ps of
        Left err -> return $ ErrorI $ convertParsecError err
        Right expr_ps -> interpretExpr expr_ps

interpretExpr :: LCalcExpr CalcPs -> Calculator Interpret
interpretExpr expr_ps = do
  result <- runExceptT $ do
    expr_rn <- runRn $ renameExpression expr_ps
    expr_tc <- runTc $ typecheckExpression expr_rn
    lift $ modify $ \s -> s {strip_override = True}
    result <- evaluate $ simplify expr_tc
    return $ case result of
      LamR _ _ -> "cannot display lambda"
      ValR v -> show v
  return $ either ErrorI ValI result

interpretDecl :: LCalcDecl CalcPs -> Calculator Interpret
interpretDecl decl_ps = do
  result <- runExceptT $ do
    decl_rn <- runRn $ renameDeclaration decl_ps
    decl_tc <- runTc $ typecheckDeclaration decl_rn
    let L _ (ValD ty (L _ uname) exp) = decl_tc
    lift $ setVariable uname $ simplify exp
  return $ either ErrorI (const Success) result


data Result =
  ValR Value |
  LamR Unique Expr
  deriving (Show)

evaluate :: Expr -> ECalculator Result
evaluate (Var uname) = getVariable uname >>= evaluate
evaluate (Lit value) = do
  state <- lift get
  return $ ValR $ if strip_override state
    then stripUnitOverride value
    else value
evaluate (Lam uname exp) = return $ LamR uname exp
evaluate (App left right) = do
  LamR uname exp <- evaluate left
  lift $ setVariable uname right
  evaluate exp
evaluate (Cast exp cast) = do
  exp_result <- evaluate exp
  lift $ modify $ \s -> s {strip_override = False}
  cast_result <- evaluate cast
  case (exp_result, cast_result) of
    (ValR exp_value, ValR cast_value) -> do
      unless (unitsEqual exp_value cast_value) $
        throwE $ L mempty "missmatched units in cast"
      return $ ValR $ castValue exp_value cast_value
    _ -> throwE $ L mempty "cannot cast a lambda expression"
evaluate (BuildIn uname) = do
  s <- lift get
  let bi = build_ins s
      Just b = M.lookup uname bi
  ValR <$> b


runRn :: Rn a -> ECalculator a
runRn rn = do
  state <- lift get
  let rns = rn_state state
  case runState (runExceptT rn) rns of
    (Left str, _) -> throwE str
    (Right a, rns') -> do
      lift $ put $ state {rn_state = rns'}
      return a

runTc :: Tc a -> ECalculator a
runTc = runRn



convertParsecError :: ParseError -> Located String
convertParsecError err =
  let pos = errorPos err
      column = sourceColumn pos
      msgs = errorMessages err
      (_, msgs1) = span (SysUnExpect "" ==) msgs
      (_, msgs2) = span (UnExpect "" ==) msgs1
      (expect, _) = span (Expect "" ==) msgs2

      commasOr [] = ""
      commasOr [m] = m
      commasOr ms = commaSep (init ms) ++ " or " ++ last ms

      commaSep [] = ""
      commaSep [m] = m
      commaSep (m : ms) = m ++ ", " ++ commaSep ms
  in L (SrcSpan column $ column + 1) $ "expected " ++ commasOr (fmap messageString expect)