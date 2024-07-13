module GCI.Renamer.Types where


import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

import Language.Calc.Syntax.Expr

import GCI.Calc.Extension

import GCI.Types.Names
import GCI.Types.SrcLoc

import Data.Word
import Data.Map as M
import Data.Maybe


type Rn = ExceptT (Located String) (State RnState)
type Tc = Rn
data RnState = RnState {
  glbState :: GlbState,
  lclState :: LclState}

data GlbState = GlbState {
  unique_counter :: Word64,
  fixities :: Map Unique Fixity,
  types :: Map Unique LType,
  unique_map :: Map String Unique}

newtype LclState = LclState {
  names :: Map String Unique}

newtype Fixity = Fixity Int
  deriving (Show, Eq, Ord)

type LType = Located Type
data Type =
  Lambda LType LType
  | Value
  | Variable Unique
  deriving (Eq)
instance Show Type where
  show Value = "Value"
  show (Variable a) = show a
  show (Lambda l r) = "(" ++ show l ++ " -> " ++ show r ++ ")"


defaultState :: RnState
defaultState = RnState {
  lclState = LclState {
    names = mempty},
  glbState = GlbState {
    unique_counter = 100,
    fixities = M.fromList [
      (Unique "+" 0, Fixity 0),
      (Unique "-" 1, Fixity 0),
      (Unique "*" 2, Fixity 1),
      (Unique "/" 3, Fixity 1),
      (Unique "^" 4, Fixity 2)],
    types = M.fromList [
      (Unique "+" 0, L mempty $ Lambda (L mempty Value) $ L mempty $ Lambda (L mempty Value) $ L mempty Value),
      (Unique "-" 1, L mempty $ Lambda (L mempty Value) $ L mempty $ Lambda (L mempty Value) $ L mempty Value),
      (Unique "*" 2, L mempty $ Lambda (L mempty Value) $ L mempty $ Lambda (L mempty Value) $ L mempty Value),
      (Unique "/" 3, L mempty $ Lambda (L mempty Value) $ L mempty $ Lambda (L mempty Value) $ L mempty Value),
      (Unique "^" 4, L mempty $ Lambda (L mempty Value) $ L mempty $ Lambda (L mempty Value) $ L mempty Value),
      (Unique "negate" 5, L mempty $ Lambda (L mempty Value) $ L mempty Value)],
    unique_map = M.fromList [
      ("+", Unique "+" 0),
      ("-", Unique "-" 1),
      ("*", Unique "*" 2),
      ("/", Unique "/" 3),
      ("^", Unique "^" 4),
      ("negate", Unique "negate" 5)]}}


mkUniqueName :: String -> Rn Unique
mkUniqueName name = do
  s <- lift get
  let glbs = glbState s
      ucs = unique_counter glbs
      uname = Unique {unique_name = name, unique_int = ucs}
  lift $ put $ s {glbState = glbs {unique_counter = ucs + 1}}
  return uname


getName :: String -> Rn (Maybe Unique)
getName name = do
  s <- lift get
  let lcls = lclState s
      glbs = glbState s 
      ns = names lcls
      um = unique_map glbs
  return $ case M.lookup name ns of
    Nothing -> M.lookup name um
    u -> u

addName :: String -> Unique -> Rn ()
addName name uname = do
  s <- lift get
  let lcls = lclState s
      ns = names lcls
  lift $ put $ s {lclState = lcls {names = insert name uname ns}}


getLocalState :: Rn LclState
getLocalState = lclState <$> lift get

putLocalState :: LclState -> Rn ()
putLocalState lcls = lift $ modify $ \s -> s {lclState = lcls}


getFixity :: Unique -> Rn Fixity
getFixity name = do
  s <- lift get
  let glbs = glbState s
      fs = fixities glbs
  return $ findWithDefault (Fixity 9) name fs


applyVariable :: Unique -> LType -> LType -> LType
applyVariable uname ty (L loc (Lambda l r)) = L loc $
  Lambda (applyVariable uname ty l) (applyVariable uname ty r)
applyVariable _ _ v@(L _ Value) = v
applyVariable name ty v@(L _ (Variable a))
  | name == a = ty
  | otherwise = v


getType :: Located Unique -> Rn LType
getType lname = do
  s <- lift get
  let glbs = glbState s
      tys = types glbs
      uname = unLoc lname
  case M.lookup uname tys of
    Nothing -> reportError (getLoc lname) $ "unbound variable " ++ unique_name uname
    Just ty -> return ty

addType :: Unique -> LType -> Rn ()
addType uname ty = do
  s <- lift get
  let glbs = glbState s
      tys = types glbs
  lift $ put $ s {glbState = glbs {
    types = M.insert uname ty tys}}

addVariable :: String -> Unique -> Rn ()
addVariable name uname = do
  s <- lift get
  let glbs = glbState s
      um = unique_map glbs
  lift $ put $ s {glbState = glbs {
    unique_map = M.insert name uname um}}


reportError :: SrcSpan -> String -> Rn a
reportError span = throwE . L span