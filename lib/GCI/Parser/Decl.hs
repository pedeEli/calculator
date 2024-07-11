module GCI.Parser.Decl where


import Text.Parsec

import Language.Calc.Syntax.Decl
import Language.Calc.Syntax.Expr
import Language.Calc.Syntax.Lit
import Language.Calc.Syntax.Extension
import GCI.Calc.Extension
import GCI.Calc.Decl
import GCI.Types.SrcLoc
import GCI.Parser.Lexer
import GCI.Parser.Expr




parseDeclaration :: Parsec String () (LCalcDecl CalcPs)
parseDeclaration = do
  spaces
  decl <- try opdec <|> vardec
  eof
  return decl


opdec :: Parsec String () (LCalcDecl CalcPs)
opdec = do
  vara <- variable
  notunits $ unLoc vara
  spaces
  op <- operator
  spaces
  varb <- variable
  notunits $ unLoc varb
  spaces
  char '='
  spaces
  exp <- infixExp
  return $ addCLoc vara exp $ ValD noExtField op $
    L (getLoc exp) $ CalcLam noExtField vara $
      L (getLoc exp) $ CalcLam noExtField varb exp


vardec :: Parsec String () (LCalcDecl CalcPs)
vardec = do
  name <- variable
  notunits $ unLoc name
  spaces
  args <- many $ do
    arg <- variable
    notunits $ unLoc arg
    spaces
    return arg
  char '='
  spaces
  exp <- infixExp
  let lam = mkLam args exp
  return $ addCLoc name exp $ ValD noExtField name lam
  where
    mkLam :: [Located String] -> LCalcExpr CalcPs -> LCalcExpr CalcPs
    mkLam [] exp = exp
    mkLam (arg : args) exp = addCLoc arg exp $
      CalcLam noExtField arg $ mkLam args exp


notunits :: String -> Parsec String () ()
notunits str = do
  let result = runParser units () "" str
  case result of
    Left _ -> return ()
    Right _ -> unexpected "units" <?> "variable"