module GCI.Parser.Expr where


import Prelude hiding (exp)

import Text.Parsec

import Language.Calc.Syntax.Expr
import Language.Calc.Syntax.Lit
import Language.Calc.Syntax.Extension
import GCI.Calc.Extension
import GCI.Calc.Expr
import GCI.Types.SrcLoc
import GCI.Parser.Lexer




parseExpression :: Parsec String () (LCalcExpr CalcPs)
parseExpression = do
  spaces
  exp <- infixExp
  cast <- optionMaybe $ do
    open <- castOpen
    L _ cast <- castexp
    close <- castClose
    return $ addCLoc open close cast
  eof
  return $ case cast of
    Nothing -> exp
    Just cast -> addCLoc exp cast $ CalcCast noExtField exp cast


infixExp' :: Parsec String () (Located String) -> Parsec String () (LCalcExpr CalcPs) -> Parsec String () (LCalcExpr CalcPs)
infixExp' operator exp = do
  exp1 <- exp
  result <- optionMaybe $ do
    op <- operator <* spaces
    exp2 <- infixExp' operator exp
    return (op, exp2)
  return $ case result of
    Nothing -> exp1
    Just (op, exp2) -> mkCalcOpApp exp1 op exp2

infixExp :: Parsec String () (Located (CalcExpr CalcPs))
infixExp = infixExp' operator fexp

fexp :: Parsec String () (LCalcExpr CalcPs)
fexp = do
  minus <- optionMaybe $ located $ char '-' <* spaces
  exp1 <- aexp
  let exp1' = maybe exp1 (mkCalcNegApp exp1) minus
  result <- optionMaybe fexp
  return $ case result of
    Nothing -> exp1'
    Just exp2 -> mkCalcApp exp1' exp2


aexp :: Parsec String () (LCalcExpr CalcPs)
aexp = valexp <|> try units <|> varexp <|> texp


texp :: Parsec String () (LCalcExpr CalcPs)
texp = do
  open <- bracketOpen
  spaces
  exp <- infixExp
  close <- bracketClose
  spaces
  return $ addCLoc open close $ CalcPar noExtField exp


varexp :: Parsec String () (LCalcExpr CalcPs)
varexp = mkCalcVar <$> (variable <* spaces)


valexp :: Parsec String () (LCalcExpr CalcPs)
valexp = do
  val <- mkCalcRational <$> value
  spaces
  us <- optionMaybe units
  return $ case us of
    Nothing -> val
    Just us -> mkCalcImpMult val us

units :: Parsec String () (LCalcExpr CalcPs)
units = do
  us <- many1 (unit <* spaces)
  rest <- optionMaybe letter
  case rest of
    Just rest -> unexpected [rest] <?> "unit"
    Nothing -> return $ foldl1 mkCalcImpMult us

unit :: Parsec String () (LCalcExpr CalcPs)
unit = do
  L loc (str, u, r, e) <- singleUnit <?> "unit"
  return $ L loc $ CalcLit noExtField $ CalcUnit noExtField str u r e


castexp :: Parsec String () (LCalcExpr CalcPs)
castexp = infixExp' castop castaexp

castop :: Parsec String () (Located String)
castop = located $ string "*" <|> string "/"

castaexp :: Parsec String () (LCalcExpr CalcPs)
castaexp = try castvalexp <|> units

castvalexp :: Parsec String () (LCalcExpr CalcPs)
castvalexp = do
  one <- located $ char '1' >> return 1
  spaces
  return $ mkCalcRational one


mkCalcOpApp :: LCalcExpr CalcPs -> Located String -> LCalcExpr CalcPs -> LCalcExpr CalcPs
mkCalcOpApp e1 op e2 = addCLoc e1 e2 $ CalcOpApp noExtField e1 op e2

mkCalcRational :: Located Rational -> LCalcExpr CalcPs
mkCalcRational = fmap (CalcLit noExtField . CalcRational noExtField)

mkCalcApp :: LCalcExpr CalcPs -> LCalcExpr CalcPs -> LCalcExpr CalcPs
mkCalcApp e1 e2 = addCLoc e1 e2 $ CalcApp noExtField e1 e2

mkCalcVar :: Located String -> LCalcExpr CalcPs
mkCalcVar v = v {unLoc = CalcVar noExtField v}

mkCalcImpMult :: LCalcExpr CalcPs -> LCalcExpr CalcPs -> LCalcExpr CalcPs
mkCalcImpMult e1 e2 = addCLoc e1 e2 $ CalcImpMult noExtField e1 e2

mkCalcNegApp :: LCalcExpr CalcPs -> Located a -> LCalcExpr CalcPs
mkCalcNegApp exp neg = addCLoc exp neg $ CalcNegApp noExtField exp