{-# LANGUAGE DeriveFunctor #-}
module GCI.Types.SrcLoc where


data SrcSpan = SrcSpan {
  srcSpanS :: !Int,
  srcSpanE :: !Int}
  deriving (Eq)
instance Show SrcSpan where
  show (SrcSpan start end) = "(" ++ show start ++ "-" ++ show end ++ ")"

data Located e = L {getLoc :: SrcSpan, unLoc :: e}
  deriving (Functor, Eq)
instance Show e => Show (Located e) where
  show (L loc e) = show loc ++ ": " ++ show e


instance Semigroup SrcSpan where
  SrcSpan s1 e1 <> SrcSpan s2 e2 = SrcSpan (min s1 s2) (max e1 e2)
instance Monoid SrcSpan where
  mempty = SrcSpan maxBound minBound

mkLocated :: Int -> Int -> e -> Located e
mkLocated start end = L (SrcSpan start end)

combineLocs :: Located a -> Located b -> SrcSpan
combineLocs a b = getLoc a <> getLoc b

addCLoc :: Located a -> Located b -> c -> Located c
addCLoc a b = L (combineLocs a b)