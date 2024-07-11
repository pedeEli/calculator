module GCI.Renamer.Types where


import Control.Monad.Trans.State

import Language.Calc.Syntax.Expr

import GCI.Calc.Extension

import Data.Word
import Data.Map as M


type Rn = State RnState
data RnState = RnState {
  glbState :: GlbState,
  lclState :: LclState}

data GlbState = GlbState {
  unique_counter :: Word64,
  fixities :: Map String Fixity}

newtype LclState = LclState {
  names :: Map String String}

newtype Fixity = Fixity Int
  deriving (Show, Eq, Ord)


defaultState :: RnState
defaultState = RnState {
  lclState = LclState {
    names = mempty},
  glbState = GlbState {
    unique_counter = 0,
    fixities = M.fromList [
      ("+", Fixity 0),
      ("-", Fixity 0),
      ("*", Fixity 1),
      ("/", Fixity 1),
      ("^", Fixity 2)]}}


mkUniqueName :: String -> Rn String
mkUniqueName name = do
  s <- get
  let glbs = glbState s
      ucs = unique_counter glbs
      uname = name ++ "_" ++ show ucs
  put $ s {glbState = glbs {unique_counter = ucs + 1}}
  return uname


getName :: String -> Rn (Maybe String)
getName name = do
  s <- get
  let lcls = lclState s
      ns = names lcls
  return $ M.lookup name ns

addName :: String -> String -> Rn ()
addName name uname = do
  s <- get
  let lcls = lclState s
      ns = names lcls
  put $ s {lclState = lcls {names = insert name uname ns}}


getLocalState :: Rn LclState
getLocalState = lclState <$> get

putLocalState :: LclState -> Rn ()
putLocalState lcls = modify $ \s -> s {lclState = lcls}


getFixity :: String -> Rn Fixity
getFixity name = do
  s <- get
  let glbs = glbState s
      fs = fixities glbs
  return $ findWithDefault (Fixity 9) name fs