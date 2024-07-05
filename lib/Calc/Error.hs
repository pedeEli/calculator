module Calc.Error where


import Text.Parsec.Error hiding (Message)
import Text.Parsec.Pos


newtype ErrorMessage = Message String

instance Show ErrorMessage where
  show (Message str) = str


data Position = Position {_start :: Int, _end :: Int}

instance Semigroup Position where
  Position s1 e1 <> Position s2 e2 = Position (min s1 s2) (max e1 e2)
instance Monoid Position where
  mempty = Position maxBound minBound
instance Show Position where
  show (Position start end) = show start ++ "-" ++ show end


data Error = Error {_pos :: Position, _source :: String , _message :: ErrorMessage}

instance Show Error where
  show (Error pos source message) = source ++ " " ++ show pos ++ ": " ++ show message




fromParsecError :: ParseError -> String -> Error
fromParsecError err source =
  let message = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages err)
      pos = sourceColumn $ errorPos err
  in Error {_pos = Position pos (pos + 1), _message = Message message, _source = source}