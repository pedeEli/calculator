module GCI.Renamer.Decl where


import Language.Calc.Syntax.Decl
import Language.Calc.Syntax.Extension

import GCI.Calc.Extension
import GCI.Calc.Decl

import GCI.Renamer.Types
import GCI.Renamer.Expr

import GCI.Types.SrcLoc


renameDeclaration :: LCalcDecl CalcPs -> Rn (LCalcDecl CalcRn)
renameDeclaration (L loc (ValD _ name exp_ps)) = do
  exp_rn <- renameExpression exp_ps
  return $ L loc $ ValD noExtField name exp_rn