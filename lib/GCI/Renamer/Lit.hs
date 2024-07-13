module GCI.Renamer.Lit where

  
import Language.Calc.Syntax.Expr
import Language.Calc.Syntax.Lit
import Language.Calc.Syntax.Extension

import GCI.Calc.Extension
import GCI.Calc.Lit

import GCI.Renamer.Types

import GCI.Types.SrcLoc


renameLit :: CalcLit CalcPs -> Rn (CalcLit CalcRn)
renameLit (CalcRational _ r) = return $ CalcRational noExtField r
renameLit (CalcUnit _ str u r e) = return $ CalcUnit noExtField str u r e