module GCI.Types.Names where


import Data.Word


data Unique = Unique {
  unique_name :: String,
  unique_int :: Word64}
  deriving (Eq, Ord)

instance Show Unique where
  show (Unique name i) = name ++ show i