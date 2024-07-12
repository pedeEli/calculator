module GCI.Renamer.Expr where

  
import Data.Maybe

import Language.Calc.Syntax.Expr
import Language.Calc.Syntax.Extension

import GCI.Calc.Extension
import GCI.Calc.Expr

import GCI.Renamer.Types
import GCI.Renamer.Lit

import GCI.Types.SrcLoc
import GCI.Types.Names


renameExpression :: LCalcExpr CalcPs -> Rn (LCalcExpr CalcRn)
renameExpression (L loc exp) = L loc <$> case exp of
  CalcVar _ name -> renameVar name
  CalcLit _ lit -> CalcLit noExtField <$> renameLit lit
  CalcLam _ name exp -> renameLam name exp
  CalcApp _ left right -> renameApp left right
  CalcNegApp _ exp -> CalcNegApp noExtField <$> renameExpression exp
  CalcOpApp _ left op right -> renameOpApp left op right
  CalcPar _ exp -> CalcPar noExtField <$> renameExpression exp
  CalcImpMult _ left right -> renameImpMult left right
  CalcCast _ left right -> renameCast left right



renameVar :: LIdP CalcPs -> Rn (CalcExpr CalcRn)
renameVar name = do
  uname <- getName $ unLoc name
  case uname of
    Nothing -> reportError $ "unknown variable " ++ unLoc name
    Just uname -> return $ CalcVar noExtField $ L (getLoc name) uname

renameLam :: LIdP CalcPs -> LCalcExpr CalcPs -> Rn (CalcExpr CalcRn)
renameLam name exp_ps = do
  backup <- getLocalState
  uname <- mkUniqueName $ unLoc name
  addName (unLoc name) uname
  exp_rn <- renameExpression exp_ps
  putLocalState backup
  return $ CalcLam noExtField (L (getLoc name) uname) exp_rn

renameApp :: LCalcExpr CalcPs -> LCalcExpr CalcPs -> Rn (CalcExpr CalcRn)
renameApp left_ps right_ps = do
  left_rn <- renameExpression left_ps
  right_rn <- renameExpression right_ps
  return $ CalcApp noExtField left_rn right_rn

renameOpApp :: LCalcExpr CalcPs -> LIdP CalcPs -> LCalcExpr CalcPs -> Rn (CalcExpr CalcRn)
renameOpApp left_ps op_ps right_ps = do
  left_rn <- renameExpression left_ps
  op_rn <- getName $ unLoc op_ps
  case op_rn of
    Nothing -> reportError $ "unknown variable " ++ unLoc op_ps
    Just op_rn -> do
      right_rn <- renameExpression right_ps
      fix <- getFixity op_rn
      return $ mkOpApp fix left_rn (L (getLoc op_ps) op_rn) right_rn

renameImpMult :: LCalcExpr CalcPs -> LCalcExpr CalcPs -> Rn (CalcExpr CalcRn)
renameImpMult left_ps right_ps = do
  left_rn <- renameExpression left_ps
  right_rn <- renameExpression right_ps
  return $ CalcImpMult noExtField left_rn right_rn

renameCast :: LCalcExpr CalcPs -> LCalcExpr CalcPs -> Rn (CalcExpr CalcRn)
renameCast left_ps right_ps = do
  left_rn <- renameExpression left_ps
  right_rn <- renameExpression right_ps
  return $ CalcCast noExtField left_rn right_rn



mkOpApp :: Fixity -> LCalcExpr CalcRn -> Located Unique -> LCalcExpr CalcRn -> CalcExpr CalcRn
mkOpApp fix1 e1 op1 e2@(L _ (CalcOpApp fix2 e2_1 op2 e2_2))
  | fix2 > fix1 = CalcOpApp fix1 e1 op1 e2
  | otherwise   = CalcOpApp fix2 (addCLoc e1 e2_1 $ CalcOpApp fix1 e1 op1 e2_1) op2 e2_2
mkOpApp fix1 e1 op e2 = CalcOpApp fix1 e1 op e2