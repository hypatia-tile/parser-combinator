module SimpleLexer where

import Data.Char (isDigit, isAlpha, isAlphaNum)
import Simple.Combinator.Parser

data Token a
  = TInt a
  | TPlus
  | TMul
  | TLParen
  | TRParen
  deriving (Show, Eq)

