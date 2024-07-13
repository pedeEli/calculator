{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module GCI.Calc.Expr where


import Language.Calc.Syntax.Extension
import Language.Calc.Syntax.Expr
import Language.Calc.Syntax.Lit

import GCI.Calc.Extension
import GCI.Calc.Lit

import GCI.Renamer.Types

import GCI.Types.SrcLoc



type instance XVar CalcPs = NoExtField
type instance XVar CalcRn = NoExtField
type instance XVar CalcTc = Type

type instance XLit CalcPs = NoExtField
type instance XLit CalcRn = NoExtField
type instance XLit CalcTc = Type

type instance XLam CalcPs = NoExtField
type instance XLam CalcRn = NoExtField
type instance XLam CalcTc = Type

type instance XApp CalcPs = NoExtField
type instance XApp CalcRn = NoExtField
type instance XApp CalcTc = Type

type instance XOpApp CalcPs = NoExtField
type instance XOpApp CalcRn = Fixity
type instance XOpApp CalcTc = Type

type instance XNegApp CalcPs = NoExtField
type instance XNegApp CalcRn = NoExtField
type instance XNegApp CalcTc = Type

type instance XPar CalcPs = NoExtField
type instance XPar CalcRn = NoExtField
type instance XPar CalcTc = Type

type instance XImpMult CalcPs = NoExtField
type instance XImpMult CalcRn = NoExtField
type instance XImpMult CalcTc = Type

type instance XCast CalcPs = NoExtField
type instance XCast CalcRn = NoExtField
type instance XCast CalcTc = Type

type instance XExpr CalcPs = DataConCantHappen
type instance XExpr CalcRn = DataConCantHappen
type instance XExpr CalcTc = DataConCantHappen



instance {-# OVERLAPS #-} (
  Show (IdP (CalcPass p)), Show (XExpr (CalcPass p))) => Show (CalcExpr (CalcPass p)) where
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



calcExprType :: LCalcExpr CalcTc -> LType
calcExprType (L loc (CalcVar ty _)) = L loc ty 
calcExprType (L loc (CalcLit ty _)) = L loc ty 
calcExprType (L loc (CalcLam ty _ _)) = L loc ty 
calcExprType (L loc (CalcApp ty _ _)) = L loc ty 
calcExprType (L loc (CalcNegApp ty _)) = L loc ty 
calcExprType (L loc (CalcOpApp ty _ _ _)) = L loc ty 
calcExprType (L loc (CalcPar ty _)) = L loc ty 
calcExprType (L loc (CalcImpMult ty _ _)) = L loc ty 
calcExprType (L loc (CalcCast ty _ _)) = L loc ty 