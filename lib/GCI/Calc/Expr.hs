{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module GCI.Calc.Expr where


import Language.Calc.Syntax.Extension
import Language.Calc.Syntax.Expr
import Language.Calc.Syntax.Lit

import GCI.Calc.Extension



type instance XVar (CalcPass _) = NoExtField
type instance XLit (CalcPass _) = NoExtField
type instance XLam (CalcPass _) = NoExtField
type instance XApp (CalcPass _) = NoExtField
type instance XOpApp (CalcPass _) = NoExtField
type instance XNegApp (CalcPass _) = NoExtField
type instance XPar (CalcPass _) = NoExtField
type instance XImpMult (CalcPass _) = NoExtField
type instance XCast (CalcPass _) = NoExtField
type instance XExpr (CalcPass _) = DataConCantHappen


type instance XCalcVal (CalcPass _) = NoExtField
type instance XCalcUnit (CalcPass _) = NoExtField
type instance XXLit (CalcPass _) = DataConCantHappen



instance {-# OVERLAPS #-} Show (CalcExpr CalcPs) where
  show (CalcVar _ id) = show id
  show (CalcLit _ lit) = show lit
  show (CalcLam _ ids exp) = "\\" ++ show ids ++ " -> " ++ show exp
  show (CalcApp _ left right) = show left ++ " " ++ show right
  show (CalcNegApp _ exp) = "-" ++ show exp
  show (CalcOpApp _ left op right) = show left ++ " " ++ show op ++ " " ++ show right
  show (CalcPar _ exp) = "(" ++ show exp ++ ")"
  show (CalcImpMult _ left right) = show left ++ show right
  show (CalcCast _ exp cast) = show exp ++ " [" ++ show cast ++ "]"
  show (XCalcExpr p) = show p

instance {-# OVERLAPS #-} Show (CalcLit CalcPs) where
  show (CalcRational _ r) = show r
  show (CalcUnit _ u e) = case e of
    Nothing -> u
    Just e -> u ++ "^" ++ show e
  show (XLit p) = show p