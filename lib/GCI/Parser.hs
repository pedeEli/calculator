{-# LANGUAGE DataKinds #-}
module GCI.Parser where


import Text.Parsec

import Language.Calc.Syntax.Expr
import Language.Calc.Syntax.Decl
import GCI.Calc.Extension


-- parseDeclaration :: Parsec String () (CalcDecl CalcPs)
-- parseDeclaration = _