module GCI.Calculator where


import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad

import Text.Parsec

import Data.Map as M
import Data.Word

import Language.Calc.Syntax.Expr
import Language.Calc.Syntax.Decl
import Language.Calc.Syntax.Lit

import GCI.Calc.Extension

import GCI.Parser.Expr
import GCI.Parser.Decl

import GCI.Renamer.Types
import GCI.Renamer.Expr
import GCI.Renamer.Decl

import GCI.Typechecker.Expr
import GCI.Typechecker.Decl

import GCI.Types.Names 
import GCI.Types.SrcLoc
import GCI.Types.Value as V

import GCI.Core.Expr


type Calculator = StateT CState IO
type ECalculator = ExceptT String Calculator
data CState = CState {
  rn_state :: RnState,
  variables :: Map Unique Expr,
  build_ins :: Map Unique (ECalculator Value),
  strip_override :: Bool}


getVariable :: Unique -> ECalculator Expr
getVariable uname = do
  s <- lift get
  let vars = variables s
  case M.lookup uname vars of
    Nothing -> throwE $ "unbound variable " ++ unique_name uname
    Just exp -> return exp

setVariable :: Unique -> Expr -> Calculator ()
setVariable uname exp = modify $ \s -> s {
  variables = M.insert uname exp $ variables s}

startCalculator :: Calculator () -> IO ()
startCalculator calc = evalStateT calc $ CState {
  strip_override = True,
  rn_state = defaultState,
  variables = M.fromList [
    (Unique "+" 0, Lam (Unique "a" 50) $ Lam (Unique "b" 50) $ BuildIn $ Unique "+" 0),
    (Unique "-" 1, Lam (Unique "a" 51) $ Lam (Unique "b" 51) $ BuildIn $ Unique "-" 1),
    (Unique "*" 2, Lam (Unique "a" 52) $ Lam (Unique "b" 52) $ BuildIn $ Unique "*" 2),
    (Unique "/" 3, Lam (Unique "a" 53) $ Lam (Unique "b" 53) $ BuildIn $ Unique "/" 3),
    (Unique "^" 4, Lam (Unique "a" 54) $ Lam (Unique "b" 54) $ BuildIn $ Unique "^" 4),
    (Unique "negate" 5, Lam (Unique "a" 55) $ BuildIn $ Unique "negate" 5)],
  build_ins = M.fromList [
    (Unique "+" 0, convertOperator 50 (<<+>>)),
    (Unique "-" 1, convertOperator 51 (<<->>)),
    (Unique "*" 2, convertOperator 52 (<<*>>)),
    (Unique "/" 3, convertOperator 53 (<</>>)),
    (Unique "^" 4, convertOperator 54 (<<^>>)),
    (Unique "negate" 5, do
      a_expr <- getVariable $ Unique "a" 55
      a_result <- evaluate a_expr
      case a_result of
        ValR a_value -> V.negate a_value
        _ -> throwE "cannot apply lambda to build in function")]}


convertOperator :: Word64 -> (Value -> Value -> ECalculator Value) -> ECalculator Value
convertOperator i op = do
  a_expr <- getVariable $ Unique "a" i
  b_expr <- getVariable $ Unique "b" i
  a_result <- evaluate a_expr
  b_result <- evaluate b_expr
  case (a_result, b_result) of
    (ValR a_value, ValR b_value) -> op a_value b_value
    _ -> throwE "cannot apply lambda to build in operator"



interpret :: String -> Calculator (Maybe String)
interpret str = do
  let decl_ps = runParser parseDeclaration () "<interactive>" str
  case decl_ps of
    Right decl_ps -> interpretDecl decl_ps
    Left _ -> do
      let expr_ps = runParser parseExpression () "<interactive>" str
      case expr_ps of
        Left err -> return $ Just $ show err
        Right expr_ps -> interpretExpr expr_ps

interpretExpr :: LCalcExpr CalcPs -> Calculator (Maybe String)
interpretExpr expr_ps = do
  result <- runExceptT $ do
    expr_rn <- runRn $ renameExpression expr_ps
    expr_tc <- runTc $ typecheckExpression expr_rn
    lift $ modify $ \s -> s {strip_override = True}
    result <- evaluate $ simplify expr_tc
    return $ case result of
      LamR _ _ -> "cannot display lambda"
      ValR v -> show v
  return $ either Just Just result

interpretDecl :: LCalcDecl CalcPs -> Calculator (Maybe String)
interpretDecl decl_ps = do
  result <- runExceptT $ do
    decl_rn <- runRn $ renameDeclaration decl_ps
    decl_tc <- runTc $ typecheckDeclaration decl_rn
    let L _ (ValD ty (L _ uname) exp) = decl_tc
    lift $ setVariable uname $ simplify exp
  return $ either Just (const Nothing) result


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
  left_r <- evaluate left
  case left_r of
    ValR _ -> throwE "cannot apply argument to value"
    LamR uname expr -> do
      lift $ setVariable uname right
      evaluate expr
evaluate (Cast exp cast) = do
  exp_result <- evaluate exp
  cast_result <- evaluate cast
  case (exp_result, cast_result) of
    (ValR exp_value, ValR cast_value) -> do
      when (unitsEqual exp_value cast_value) $
        throwE "missmatched units in cast"
      return $ ValR $ castValue exp_value cast_value
    _ -> throwE "cannot cast a lambda expression"
evaluate (BuildIn uname) = do
  s <- lift get
  let bi = build_ins s
  case M.lookup uname bi of
    Nothing -> throwE $ "unknown build in " ++ unique_name uname
    Just b -> ValR <$> b


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