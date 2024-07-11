module GCI.Test where


import Language.Calc.Syntax.Decl

import Control.Monad.Trans.State

import Text.Parsec

import GCI.Parser.Expr
import GCI.Parser.Decl

import GCI.Renamer.Expr
import GCI.Renamer.Decl
import GCI.Renamer.Types


test :: Parsec String () a -> (a -> Rn b) -> String -> Either ParseError b
test parser renamer str = case runParser parser () "" str of
  Left err -> Left err
  Right result -> Right $ evalState (renamer result) defaultState

testDecl = test parseDeclaration renameDeclaration
testExpr = test parseExpression renameExpression