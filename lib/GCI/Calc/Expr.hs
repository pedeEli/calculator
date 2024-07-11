{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module GCI.Calc.Expr where


import Language.Calc.Syntax.Extension
import Language.Calc.Syntax.Expr
import Language.Calc.Syntax.Lit

import GCI.Calc.Extension
import GCI.Calc.Lit

import GCI.Renamer.Types

import GCI.Types.SrcLoc



type instance XVar (CalcPass _) = NoExtField
type instance XLit (CalcPass _) = NoExtField
type instance XLam (CalcPass _) = NoExtField
type instance XApp (CalcPass _) = NoExtField

type instance XOpApp CalcPs = NoExtField
type instance XOpApp CalcRn = Fixity
type instance XOpApp CalcTc = NoExtField

type instance XNegApp (CalcPass _) = NoExtField
type instance XPar (CalcPass _) = NoExtField
type instance XImpMult (CalcPass _) = NoExtField
type instance XCast (CalcPass _) = NoExtField
type instance XExpr (CalcPass _) = DataConCantHappen



instance {-# OVERLAPS #-} Show (IdP (CalcPass p)) => Show (CalcExpr (CalcPass p)) where
  show (CalcVar _ id) = show (unLoc id)
  show (CalcLit _ lit) = show lit
  show (CalcLam _ id exp) = "(\\" ++ show (unLoc id) ++ " -> " ++ show (unLoc exp) ++ ")"
  show (CalcApp _ left right) = "(" ++ show (unLoc left) ++ " " ++ show (unLoc right) ++ ")"
  show (CalcNegApp _ exp) = "(-" ++ show (unLoc exp) ++ ")"
  show (CalcOpApp _ left op right) = "(" ++ show (unLoc left) ++ " " ++ show (unLoc op) ++ " " ++ show (unLoc right) ++ ")"
  show (CalcPar _ exp) = "(" ++ show (unLoc exp) ++ ")"
  show (CalcImpMult _ left right) = "(" ++ show (unLoc left) ++ show (unLoc right) ++ ")"
  show (CalcCast _ exp cast) = show (unLoc exp) ++ " [" ++ show (unLoc cast) ++ "]"
  show (XCalcExpr p) = show p