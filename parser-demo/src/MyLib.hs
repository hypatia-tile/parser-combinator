-- Create various parser here, experement and search my better way to write parser
module MyLib (someFunc) where

import Data.List.NonEmpty
import Data.Char
import qualified Data.List as L (span)

someFunc :: IO ()
someFunc = test

-- Simple Arithmetic Token
data Token
  = TokNumber Int
  | TokAdd
  | TokSub
  | TokMul
  | TokDiv
  | TokEof
  deriving (Show, Eq)

-- Re-invent ReadP
data Parser a
  = Get (Char -> Parser a)
  | Look (String -> Parser a)
  | Fail
  | Result a (Parser a)
  | Final (NonEmpty (a, String))

newtype ReadParser a = R (forall b. (a -> Parser b) -> Parser b)

instance Functor ReadParser where
  -- Let b be a funcion from a to c, and show that
  --   R g = h (R f) :: ReadParser c is defined,
  --   i.e. g :: ((c -> Parser b) -> Parser b).
  -- Since f :: ((a -> Parser b) -> Parser b),
  --   if h :: a -> c, and k is given,
  --   k . h :: a -> Parser b
  --   k :: c -> Parser b
  fmap h (R f) = R (\k -> f (k . h))

parseInt :: String -> Parser Token
parseInt [] = Final (singleton (TokEof, []))
parseInt source = let (numpart, rest) = L.span isDigit source
                   in case numpart of
                     [] -> parseOp source
                     _ -> Result (TokNumber (read numpart)) $ parseInt rest

parseOp :: String -> Parser Token
parseOp ('+': rest) = Result TokAdd (parseOp rest)
parseOp ('-': rest) = Result TokSub (parseOp rest)
parseOp ('*': rest) = Result TokMul (parseOp rest)
parseOp ('/': rest) = Result TokDiv (parseOp rest)
parseOp _ = Fail


checkResult :: String -> IO ()
checkResult source = do
  case parseInt source of
    Result tok _newParser -> print tok
    _ -> fail "Unsupported behavior"

test :: IO ()
test = do
  checkResult "++"
  checkResult "-+"
  checkResult "/+"
  checkResult "*+"
  checkResult "1+"



