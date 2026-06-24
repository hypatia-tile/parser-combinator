module SimpleLexer where

import Data.Char (isDigit, isAlpha, isAlphaNum)
import Simple.Combinator.Parser
import Simple.Data.AExp

data Token a
  = TInt a
  | TPlus
  | TMul
  | TLParen
  | TRParen
  deriving (Show, Eq)

