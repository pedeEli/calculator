{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
module Language.Calc.Syntax.Decl where


import Language.Calc.Syntax.Extension
import Language.Calc.Syntax.Expr


type LCalcDecl p = XRec p (CalcDecl p)
data CalcDecl p =
  ValD (XValD p) (LIdP p) (LCalcExpr p)


deriving instance (
  Show (XValD p), Show (LIdP p), Show (LCalcExpr p)) => Show (CalcDecl p)