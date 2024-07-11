{-# LANGUAGE TypeFamilies #-}
module GCI.Calc.Decl where


import Language.Calc.Syntax.Extension
import Language.Calc.Syntax.Expr
import Language.Calc.Syntax.Lit

import GCI.Calc.Extension


type instance XValD (CalcPass p) = NoExtField