module GCI.Typechecker.Expr where


import Data.Maybe
import Data.Foldable

import Language.Calc.Syntax.Expr
import Language.Calc.Syntax.Lit
import Language.Calc.Syntax.Extension

import GCI.Calc.Extension
import GCI.Calc.Expr

import GCI.Renamer.Types

import GCI.Typechecker.Lit

import GCI.Types.SrcLoc
import GCI.Types.Names


typecheckExpression :: LCalcExpr CalcRn -> Tc (LCalcExpr CalcTc)
typecheckExpression (L loc exp) = L loc <$> case exp of
  CalcVar _ name -> typecheckVar name
  CalcLit _ lit -> CalcLit Value <$> typecheckLit lit
  CalcLam _ name exp -> typecheckLam name exp
  CalcApp _ left right -> typecheckApp left right
  CalcNegApp _ exp -> typecheckNegApp exp
  CalcOpApp _ left op right -> typecheckOpApp left op right
  CalcPar _ exp -> typecheckPar exp
  CalcImpMult _ left right -> typecheckImpMult left right
  CalcCast _ left right -> typecheckCast left right


typecheckVar :: LIdP CalcRn -> Tc (CalcExpr CalcTc)
typecheckVar lname = do
  ty <- getType lname
  return $ CalcVar (unLoc ty) lname

typecheckLam :: LIdP CalcRn -> LCalcExpr CalcRn -> Tc (CalcExpr CalcTc)
typecheckLam lname exp_rn = do
  let name = unLoc lname
  addType name $ L (getLoc lname) $ Variable name
  exp_tc <- typecheckExpression exp_rn
  name_type <- getType lname
  let ty = Lambda name_type $ calcExprType exp_tc
  return $ CalcLam ty lname exp_tc

typecheckApp :: LCalcExpr CalcRn -> LCalcExpr CalcRn -> Tc (CalcExpr CalcTc)
typecheckApp left_rn right_rn = do
  left_tc <- typecheckExpression left_rn
  right_tc <- typecheckExpression right_rn
  let left_ty  = calcExprType left_tc
      right_ty = calcExprType right_tc
  ty <- applyType left_ty right_ty
  return $ CalcApp (unLoc ty) left_tc right_tc

typecheckNegApp :: LCalcExpr CalcRn -> Tc (CalcExpr CalcTc)
typecheckNegApp exp_rn = do
  exp_tc <- typecheckExpression exp_rn
  let ty = calcExprType exp_tc
      loc = getLoc exp_tc
  uname <- getName "negate"
  case uname of
    Nothing -> reportError loc "negate is not defined"
    Just uname -> return $
      CalcApp (unLoc ty) (L loc $ CalcVar (Variable uname) (L loc uname)) exp_tc

typecheckOpApp :: LCalcExpr CalcRn -> Located Unique -> LCalcExpr CalcRn -> Tc (CalcExpr CalcTc)
typecheckOpApp left_rn op right_rn = do
  left_tc <- typecheckExpression left_rn
  right_tc <- typecheckExpression right_rn
  let left_ty  = calcExprType left_tc
      right_ty = calcExprType right_tc
  op_ty <- getType op
  ty1 <- applyType op_ty left_ty
  ty2 <- applyType ty1 right_ty
  return $ CalcOpApp (unLoc ty2) right_tc op left_tc

typecheckPar :: LCalcExpr CalcRn -> Tc (CalcExpr CalcTc)
typecheckPar exp_rn = do
  exp_tc <- typecheckExpression exp_rn
  let ty = calcExprType exp_tc
  return $ CalcPar (unLoc ty) exp_tc

typecheckImpMult :: LCalcExpr CalcRn -> LCalcExpr CalcRn -> Tc (CalcExpr CalcTc)
typecheckImpMult left_rn right_rn = do
  left_tc <- typecheckExpression left_rn
  right_tc <- typecheckExpression right_rn
  let left_ty  = calcExprType left_tc
      right_ty = calcExprType right_tc
      left_loc  = getLoc left_tc
      right_loc = getLoc right_tc
  uname <- getName "*"
  case (unLoc left_ty, unLoc right_ty, uname) of
    (_, _, Nothing) -> reportError (left_loc <> right_loc) "* is not defined"
    (Value, Value, Just uname) -> return $
      CalcOpApp Value left_tc (L (left_loc <> right_loc) uname) right_tc
    (Value, _, _) -> reportError right_loc "implicit multiplication only possible with value types"
    _ -> reportError left_loc "implicit multiplication only possible with value types"

typecheckCast :: LCalcExpr CalcRn -> LCalcExpr CalcRn -> Tc (CalcExpr CalcTc)
typecheckCast exp_rn cast_rn = do
  exp_tc  <- typecheckExpression exp_rn
  cast_tc <- typecheckExpression cast_rn
  let exp_ty  = calcExprType exp_tc
      cast_ty = calcExprType cast_tc
  case (unLoc exp_ty, unLoc cast_ty) of
    (Value, Value) -> return $ CalcCast Value exp_tc cast_tc
    (Value, _) -> reportError (getLoc cast_tc) "cast cannot be a function"
    _ -> reportError (getLoc exp_tc) "cast cannot be applied to function"


applyType :: LType -> LType -> Tc LType
applyType (L loc Value) _ = reportError loc "cannot apply argument to value"
applyType (L loc (Variable a)) r = do
  return_type_name <- mkUniqueName $ unique_name a
  let return_type = L loc $ Variable return_type_name
      ty = L (loc <> getLoc r) $ Lambda r return_type
  addType return_type_name return_type
  addType a ty
  return return_type
applyType (L loc (Lambda arg right)) app = do
  eqs <- typesEqual arg app
  traverse_ (uncurry addType) eqs
  return $ foldr (uncurry applyVariable) right eqs


typesEqual :: LType -> LType -> Tc [(Unique, LType)]
typesEqual (L _ Value) (L _ Value) = return []
typesEqual v@(L _ Value) (L _ (Variable uname)) = return [(uname, v)]
typesEqual (L loc Value) ty = reportError (loc <> getLoc ty) "missmatching types"
typesEqual (L _ (Variable uname)) ty = return [(uname, ty)]
typesEqual (L _ (Lambda ty1_1 ty1_2)) (L _ (Lambda ty2_1 ty2_2)) = do
  eqs1 <- typesEqual ty1_1 ty2_1
  eqs2 <- typesEqual ty1_2 ty2_2
  return $ eqs1 ++ eqs2
typesEqual (L loc (Lambda _ _)) ty = reportError (loc <> getLoc ty) "missmatching types" 