{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances, MonoLocalBinds #-}
module Language.Calc.Syntax.Expr where


import Language.Calc.Syntax.Extension
import Language.Calc.Syntax.Lit


type LCalcExpr p = XRec p (CalcExpr p)
data CalcExpr p =
  CalcVar (XVar p) (LIdP p) |
  CalcLit (XLit p) (CalcLit p) |
  CalcLam (XLam p) (LIdP p) (LCalcExpr p) |
  CalcApp (XApp p) (LCalcExpr p) (LCalcExpr p) |
  CalcNegApp (XNegApp p) (LCalcExpr p) |
  CalcOpApp (XOpApp p) (LCalcExpr p) (LCalcExpr p) (LCalcExpr p) |
  CalcPar (XPar p) (LCalcExpr p) |
  CalcImpMult (XImpMult p) (LCalcExpr p) (LCalcExpr p) |
  CalcCast (XCast p) (LCalcExpr p) (LCalcExpr p) |
  XCalcExpr (XExpr p)
  
deriving instance (
  Show (LIdP p), Show (LCalcExpr p), Show (XVar p), Show (XLit p),
  Show (XLam p), Show (XApp p), Show (XOpApp p), Show (XImpMult p),
  Show (XCast p), Show (XExpr p), Show (CalcLit p),
  Show (XNegApp p), Show (XPar p)) => Show (CalcExpr p)