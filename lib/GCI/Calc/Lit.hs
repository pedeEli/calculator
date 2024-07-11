{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module GCI.Calc.Lit where


import Language.Calc.Syntax.Extension
import Language.Calc.Syntax.Lit

import GCI.Calc.Extension


type instance XCalcVal (CalcPass _) = NoExtField
type instance XCalcUnit (CalcPass _) = NoExtField
type instance XXLit (CalcPass _) = DataConCantHappen


instance {-# OVERLAPS #-} Show (CalcLit (CalcPass p)) where
  show (CalcRational _ r) = show r
  show (CalcUnit _ u e) = case e of
    Nothing -> u
    Just e -> u ++ "^" ++ show e
  show (XLit p) = show p