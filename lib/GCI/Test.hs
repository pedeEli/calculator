module GCI.Test where


import Language.Calc.Syntax.Decl

import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import Text.Parsec

import GCI.Parser.Expr
import GCI.Parser.Decl

import GCI.Renamer.Expr
import GCI.Renamer.Decl
import GCI.Renamer.Types

import GCI.Typechecker.Expr
import GCI.Typechecker.Decl


test :: Parsec String () a -> (a -> Rn b) -> (b -> Tc c) -> String -> Either String c
test parser renamer typechecker str = case runParser parser () "" str of
  Left err -> Left $ show err
  Right a -> case runState (runExceptT (renamer a)) defaultState of
    (Left err, _) -> Left err
    (Right b, state) -> evalState (runExceptT (typechecker b)) state

testDecl = test parseDeclaration renameDeclaration typecheckDeclaration
testExpr = test parseExpression renameExpression typecheckExpression