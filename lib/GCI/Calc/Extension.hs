{-# LANGUAGE DataKinds, GADTs, StandaloneKindSignatures, TypeFamilies #-}
module GCI.Calc.Extension where

import Data.Kind

import Language.Calc.Syntax.Extension

import GCI.Types.SrcLoc


data Pass = Parsed | Renamed | Typechecked


type CalcPass :: Pass -> Type
data CalcPass c where
  CalcPs :: CalcPass 'Parsed
  CalcRn :: CalcPass 'Renamed
  CalcTc :: CalcPass 'Typechecked

type CalcPs = CalcPass 'Parsed
type CalcRn = CalcPass 'Renamed
type CalcTc = CalcPass 'Typechecked


type instance XRec (CalcPass p) a = Located a


type instance IdP (CalcPass p) = IdCalcP p
type IdCalcP :: Pass -> Type
type family IdCalcP p where
  IdCalcP 'Parsed = String
  IdCalcP 'Renamed = String
  IdCalcP 'Typechecked = String