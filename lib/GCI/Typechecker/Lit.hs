module GCI.Typechecker.Lit where

  
import Language.Calc.Syntax.Expr
import Language.Calc.Syntax.Lit
import Language.Calc.Syntax.Extension

import GCI.Calc.Extension
import GCI.Calc.Lit

import GCI.Renamer.Types

import GCI.Types.SrcLoc


typecheckLit :: CalcLit CalcRn -> Tc (CalcLit CalcTc)
typecheckLit (CalcRational _ r) = return $ CalcRational noExtField r
typecheckLit (CalcUnit _ str u r e) = return $ CalcUnit noExtField str u r e