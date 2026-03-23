module SimpleLexer where

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

data Token
  = TInt Int
  | TPlus
  | TMul
  | TLParen
  | TRParen
  deriving (Show, Eq)

-- Parse One token, skipping leading whitespaces.
tokenP :: ReadP Token
tokenP = skipSpaces *> choice
  [ TInt . read <$> munch1 isDigit
  , TPlus <$ char '+'
  , TMul <$ char '*'
  , TLParen <$ char '('
  , TRParen <$ char ')'
  ]

-- Parse all tokens until end of input.
tokensP :: ReadP [Token]
tokensP = many tokenP <* skipSpaces <* eof

lexArithmetic :: String -> Either String [Token]
lexArithmetic input =
  case readP_to_S tokensP input of
    [] -> Left "lexing failed"
    xs ->
      case [ts | (ts, rest) <- xs, rest == ""] of
        [] -> Left "lexing failed: unconsumed input"
        ts : _ -> Right ts

