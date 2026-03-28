module Example01.ParserM where

import Control.Applicative
import Control.Monad

newtype ParserM a = P { runParserM :: String -> [(a, String)] }

instance Functor ParserM where
  fmap f (P p) = P $ \inp -> fmap (\(a, rest) -> (f a, rest)) $ p inp

instance Applicative ParserM where
  pure = result
  (P f) <*> (P p) = P $ \inp -> do
       (f', inp') <- f inp
       (x, inp'') <- p inp'
       return (f' x, inp'')

instance Monad ParserM where
  return      = pure
  (P p) >>= f = P $ \inp -> do
      (x, inp') <- p inp
      runParserM (f x) inp'

instance Alternative ParserM where
  empty = P $ \_ -> []
  P p <|> P q = P $ \inp ->
    p inp ++ q inp

instance MonadPlus ParserM where
  mzero = empty
  mplus = (<|>)

result :: a -> ParserM a
result x = P $ \inp -> [(x, inp)]

zero :: ParserM a
zero = P $ \_inp -> []

item :: ParserM Char
item = P $ \inp -> case inp of
  [] -> []
  (x : xs) -> [(x, xs)]

sat :: (Char -> Bool) -> ParserM Char
sat p = do
  x <- item
  if p x
    then result x
    else zero

char :: Char -> ParserM Char
char x = sat (== x)

digit :: ParserM Char
digit = sat (\x -> '0' <= x && x <= '9')

lower :: ParserM Char
lower = sat (\x -> 'a' <= x && x <= 'z')

upper :: ParserM Char
upper = sat (\x -> 'A' <= x && x <= 'Z')

letter :: ParserM Char
letter = lower `mplus` upper

alphanum :: ParserM Char
alphanum = letter `mplus` digit

word :: ParserM String
word = neWord `mplus` result ""
  where
    neWord = do
      x <- letter
      xs <- word
      pure (x:xs)

string :: String -> ParserM String
string inp = do
  case inp of
    "" -> return inp
    x : xs  -> do
      _ <- char x
      _ <- string xs
      return (x:xs)

test = runParserM . string
