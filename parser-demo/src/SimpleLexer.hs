module SimpleLexer where

import Text.ParserCombinators.ReadP
import Data.Char (isDigit, isAlpha, isAlphaNum)

data Token
  = TInt Int
  | TPlus
  | TMul
  | TLParen
  | TRParen
  | TIdent String
  deriving (Show, Eq)

identP :: ReadP Token
identP = TIdent <$> ((:) <$> satisfy isAlpha <*> munch isAlphaNum)

-- Parse One token, skipping leading whitespaces.
tokenP :: ReadP Token
tokenP = skipSpaces *>
  ( (TInt . read <$> munch1 isDigit)
    <++ identP
    <++ (TPlus <$ char '+')
    <++ (TMul <$ char '*')
    <++ (TLParen <$ char '(')
    <++ (TRParen <$ char ')')
  )

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

tokenStep :: String -> Maybe (Token, String)
tokenStep input =
  case readP_to_S tokenP input of
    [] -> Nothing
    xs -> case reverse xs of
      [] -> Nothing
      ((tok, rest) : _) -> Just (tok, rest)

