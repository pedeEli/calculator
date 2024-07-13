{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
module Language.Calc.Syntax.Lit where


import Language.Calc.Syntax.Extension

import GCI.Types.Unit


data CalcLit p =
  CalcRational (XCalcVal p) Rational |
  CalcUnit (XCalcUnit p) String (Unit SIUnit) Rational (Maybe Integer) |
  XLit (XXLit p)

deriving instance (
  Show (XCalcVal p), Show (XCalcUnit p),
  Show (XXLit p)) => Show (CalcLit p)