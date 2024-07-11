{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
module Language.Calc.Syntax.Lit where


import Language.Calc.Syntax.Extension


data CalcLit p =
  CalcRational (XCalcVal p) Rational |
  CalcUnit (XCalcUnit p) String (Maybe Int) |
  XLit (XXLit p)

deriving instance (
  Show (XCalcVal p), Show (XCalcUnit p),
  Show (XXLit p)) => Show (CalcLit p)