module Calc.Error where


import Text.Parsec.Error (errorPos, ParseError, errorMessages, showErrorMessages)
import Text.Parsec.Pos (sourceColumn)


newtype ErrorMessage = Message String

instance Show ErrorMessage where
  show (Message str) = str


data Position = Position {_pStart :: Int, _pEnd :: Int}

instance Semigroup Position where
  Position s1 e1 <> Position s2 e2 = Position (min s1 s2) (max e1 e2)
instance Monoid Position where
  mempty = Position maxBound minBound
instance Show Position where
  show (Position start end) = show start ++ "-" ++ show end


data Error = Error {_ePos :: Position, _eMessage :: ErrorMessage}

instance Show Error where
  show (Error pos message) = show pos ++ ": " ++ show message




fromParsecError :: ParseError -> Error
fromParsecError err =
  let message = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages err)
      pos = sourceColumn $ errorPos err
  in Error {_ePos = Position pos (pos + 1), _eMessage = Message message}