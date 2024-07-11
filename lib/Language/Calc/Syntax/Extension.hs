{-# LANGUAGE EmptyCase, StandaloneKindSignatures, TypeFamilyDependencies #-}
module Language.Calc.Syntax.Extension where


import Data.Kind


data NoExtField = NoExtField
  deriving (Show)
noExtField :: NoExtField
noExtField = NoExtField

data DataConCantHappen
dataConCantHappen :: DataConCantHappen -> a
dataConCantHappen x = case x of {}
instance Show DataConCantHappen where
  show = dataConCantHappen


type XRec :: Type -> Type -> Type
type family XRec p a = r | r -> a


type family IdP p
type LIdP p = XRec p (IdP p) 


type family XVar p
type family XLit p
type family XLam p
type family XApp p
type family XOpApp p
type family XNegApp p
type family XImpMult p
type family XPar p
type family XCast p
type family XExpr p


type family XCalcVal p
type family XCalcUnit p
type family XXLit p


type family XValD p