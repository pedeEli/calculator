{-# LANGUAGE TypeFamilies #-}
module GCI.Calc.Decl where


import Language.Calc.Syntax.Extension
import Language.Calc.Syntax.Expr
import Language.Calc.Syntax.Decl
import Language.Calc.Syntax.Lit

import GCI.Calc.Extension

import GCI.Renamer.Types


type instance XValD CalcPs = NoExtField
type instance XValD CalcRn = NoExtField
type instance XValD CalcTc = Type


calcDeclType :: CalcDecl CalcTc -> Type
calcDeclType (ValD ty _ _) = ty