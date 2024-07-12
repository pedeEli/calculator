module GCI.Typechecker.Expr where


import Data.Maybe

import Language.Calc.Syntax.Expr
import Language.Calc.Syntax.Lit
import Language.Calc.Syntax.Extension

import GCI.Calc.Extension
import GCI.Calc.Expr

import GCI.Renamer.Types

import GCI.Typechecker.Lit

import GCI.Types.SrcLoc
import GCI.Types.Names
import Debug.Trace (traceM)


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
  let name = unLoc lname
  ty <- getType name
  return $ CalcVar ty lname

typecheckLam :: LIdP CalcRn -> LCalcExpr CalcRn -> Tc (CalcExpr CalcTc)
typecheckLam lname exp_rn = do
  let name = unLoc lname
  addType name (Variable name)
  exp_tc <- typecheckExpression exp_rn
  name_type <- getType name
  let ty = Lambda name_type $ calcExprType $ unLoc exp_tc
  return $ CalcLam ty lname exp_tc

typecheckApp :: LCalcExpr CalcRn -> LCalcExpr CalcRn -> Tc (CalcExpr CalcTc)
typecheckApp left_rn right_rn = do
  left_tc <- typecheckExpression left_rn
  right_tc <- typecheckExpression right_rn
  let left_ty  = calcExprType $ unLoc left_tc
      right_ty = calcExprType $ unLoc right_tc
  ty <- applyType left_ty right_ty
  return $ CalcApp ty left_tc right_tc

typecheckNegApp :: LCalcExpr CalcRn -> Tc (CalcExpr CalcTc)
typecheckNegApp exp_rn = do
  exp_tc <- typecheckExpression exp_rn
  let ty = calcExprType $ unLoc exp_tc
  return $ CalcNegApp ty exp_tc

typecheckOpApp :: LCalcExpr CalcRn -> Located Unique -> LCalcExpr CalcRn -> Tc (CalcExpr CalcTc)
typecheckOpApp left_rn op right_rn = do
  left_tc <- typecheckExpression left_rn
  right_tc <- typecheckExpression right_rn
  let left_ty  = calcExprType $ unLoc left_tc
      right_ty = calcExprType $ unLoc right_tc
  op_ty <- getType $ unLoc op
  ty1 <- applyType op_ty left_ty
  ty2 <- applyType ty1 right_ty
  return $ CalcOpApp ty2 right_tc op left_tc

typecheckPar :: LCalcExpr CalcRn -> Tc (CalcExpr CalcTc)
typecheckPar exp_rn = do
  exp_tc <- typecheckExpression exp_rn
  let ty = calcExprType $ unLoc exp_tc
  return $ CalcPar ty exp_tc

typecheckImpMult :: LCalcExpr CalcRn -> LCalcExpr CalcRn -> Tc (CalcExpr CalcTc)
typecheckImpMult left_rn right_rn = do
  left_tc <- typecheckExpression left_rn
  right_tc <- typecheckExpression right_rn
  let left_ty  = calcExprType $ unLoc left_tc
      right_ty = calcExprType $ unLoc right_tc
  if left_ty == Value && right_ty == Value
    then return $ CalcImpMult Value left_tc right_tc
    else reportError "missmatching units in implicit multiplication"

typecheckCast :: LCalcExpr CalcRn -> LCalcExpr CalcRn -> Tc (CalcExpr CalcTc)
typecheckCast exp_rn cast_rn = do
  exp_tc  <- typecheckExpression exp_rn
  cast_tc <- typecheckExpression cast_rn
  let exp_ty  = calcExprType $ unLoc exp_tc
      cast_ty = calcExprType $ unLoc cast_tc
  if exp_ty == Value && cast_ty == Value
    then return $ CalcCast Value exp_tc cast_tc
    else reportError "mismatching units in cast"



applyType :: Type -> Type -> Tc Type
applyType Value _ = reportError "cannot apply argument to value"
applyType (Variable a) r = do
  return_type_name <- mkUniqueName $ unique_name a
  let return_type = Variable return_type_name
      ty = Lambda r return_type
  addType return_type_name return_type
  addType a ty
  return return_type
applyType (Lambda (Variable a) r1) r2 = do
  addType a r2
  return $ applyVariable a r2 r1
applyType (Lambda l r) (Variable a) = do
  addType a l
  return r
applyType (Lambda l r1) r2 = if l == r2
  then return r1
  else reportError "unmatching type"


-- test x y = x y