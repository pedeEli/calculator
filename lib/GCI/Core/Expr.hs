module GCI.Core.Expr where


import Language.Calc.Syntax.Expr
import Language.Calc.Syntax.Lit

import GCI.Calc.Extension

import GCI.Types.Names
import GCI.Types.SrcLoc

import GCI.Types.Value as V
import GCI.Types.Unit


data Expr =
  Var Unique |
  Lit Value |
  Lam Unique Expr |
  App Expr Expr |
  Cast Expr Expr |
  BuildIn Unique
  deriving (Show)


simplify :: LCalcExpr CalcTc -> Expr
simplify = simplify' . unLoc


simplify' :: CalcExpr CalcTc -> Expr
simplify' (CalcVar _ lname) = Var $ unLoc lname
simplify' (CalcLit _ lit) = simplifyLit lit
simplify' (CalcLam _ left right) = Lam (unLoc left) (simplify right)
simplify' (CalcApp _ left right) = App (simplify left) (simplify right)
simplify' (CalcOpApp _ left op right) = App (App (Var $ unLoc op) $ simplify left) $ simplify right
simplify' (CalcPar _ exp) = simplify exp
simplify' (CalcCast _ exp cast) = simplify exp


simplifyLit :: CalcLit CalcTc -> Expr
simplifyLit (CalcRational _ r) = Lit $ V.fromRational r
simplifyLit (CalcUnit _ str u r e) = Lit $ fromUnit str u r e