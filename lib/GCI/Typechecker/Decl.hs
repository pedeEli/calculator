module GCI.Typechecker.Decl where


import Language.Calc.Syntax.Decl
import Language.Calc.Syntax.Extension

import GCI.Calc.Extension
import GCI.Calc.Expr
import GCI.Calc.Decl

import GCI.Renamer.Types

import GCI.Typechecker.Expr

import GCI.Types.SrcLoc


typecheckDeclaration :: LCalcDecl CalcRn -> Tc (LCalcDecl CalcTc)
typecheckDeclaration (L loc (ValD _ lname exp_rn)) = do
  exp_tc <- typecheckExpression exp_rn
  let uname = unLoc lname
      ty = calcExprType $ unLoc exp_tc
  addType uname ty
  return $ L loc $ ValD ty lname exp_tc