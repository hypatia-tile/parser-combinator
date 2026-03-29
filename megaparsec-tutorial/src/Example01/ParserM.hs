{-# LANGUAGE GADTs #-}
module Example01.ParserM where

import Control.Applicative
import Control.Monad (MonadPlus (..))

newtype ParserM a = P {runParserM :: String -> [(a, String)]}

instance Functor ParserM where
  fmap f (P p) = P $ \inp -> fmap (\(a, rest) -> (f a, rest)) $ p inp

instance Applicative ParserM where
  pure = result
  (P f) <*> (P p) = P $ \inp -> do
    (f', inp') <- f inp
    (x, inp'') <- p inp'
    return (f' x, inp'')

instance Monad ParserM where
  return = pure
  (P p) >>= f = P $ \inp -> do
    (x, inp') <- p inp
    runParserM (f x) inp'

instance Alternative ParserM where
  empty = P $ \_ -> []
  P p <|> P q = P $ \inp ->
    take 1 $ p inp ++ q inp

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

string :: String -> ParserM String
string inp = do
  case inp of
    "" -> return inp
    x : xs -> do
      _ <- char x
      _ <- string xs
      return (x : xs)

many' :: ParserM a -> ParserM [a]
many' p = (`mplus` return []) $ do
  x <- p
  xs <- many' p
  return (x : xs)

many1 :: ParserM a -> ParserM [a]
many1 p = do
  x <- p
  xs <- many' p
  return (x : xs)

word :: ParserM String
word = many' letter

ident :: ParserM String
ident = do
  x <- lower
  xs <- many alphanum
  return (x : xs)

nat :: ParserM Int
nat = read <$> many1 digit

int :: ParserM Int
int = do
  (op :: Int -> Int) <- (const negate <$> char '-') `mplus` (return id)
  n <- nat
  return $ op n

-- Repetition with separators
sepby1 :: ParserM a -> ParserM b -> ParserM [a]
p `sepby1` q = do
  x <- p
  xs <- many' $ do _ <- q; p
  return (x : xs)

sepby :: ParserM a -> ParserM b -> ParserM [a]
p `sepby` sep = (p `sepby1` sep) `mplus` (return [])

bracket :: ParserM a -> ParserM b -> ParserM c -> ParserM b
bracket open p close = do
  _ <- open
  x <- p
  _ <- close
  return x

ints :: ParserM [Int]
ints =
  bracket
    (char '[')
    (sepby1 int (char ','))
    (char ']')

-- Repetition with meaningful separators

chainl1 :: ParserM a -> ParserM (a -> a -> a) -> ParserM a
p `chainl1` op = p >>= rest
  where
    rest x = (`mplus` return x) $ do
      f <- op
      y <- p
      rest (f x y)

-- This grammar can be translated directly into a combinator parser:
--   expr   ::= expr addop factor | factor
--   addop  ::= +  |  -
--   factor ::= nat | ( expr )
expr :: ParserM Int
expr = term `chainl1` addop

addop :: ParserM (Int -> Int -> Int)
addop = ((+) <$ char '+') `mplus` ((-) <$ char '-') 

expop :: ParserM (Int -> Int -> Int)
expop = (^) <$ char '^'

term :: ParserM Int
term = factor `chainl1` expop

factor :: ParserM Int
factor = nat `mplus` bracket (char '(') expr (char ')')

data AExp expr where
  AInt  :: Int -> AExp Int
  ABool :: Bool -> AExp Bool
  AAdd  :: AExp Int -> AExp Int -> AExp Int
  AIf   :: AExp Bool -> AExp expr -> AExp expr -> AExp expr

eval :: AExp a -> a
eval (AInt x) = x
eval (ABool b) = b
eval (AAdd x y) = eval x + eval y
eval (AIf b x y) = if eval b then eval x else eval y

spaces :: ParserM ()
spaces = do
  _ <- many1 (sat isSpace)
  return ()
  where
    isSpace x =    (x == ' ')
                || (x == '\n')
                || (x == '\t')

comment :: ParserM ()
comment = do
  _ <- string "--"
  _ <- many (sat (/= '\n'))
  return ()

junk :: ParserM ()
junk = do
  _ <- many (spaces `mplus` comment)
  return ()

parse :: ParserM a -> ParserM a
parse p = do { _ <- junk; p }

token :: ParserM a -> ParserM a
token p = do { v <- p; _ <- junk; return v }

natural :: ParserM Int
natural = token nat

integer :: ParserM Int
integer = token int

symbol :: String -> ParserM String
symbol xs = token (string xs)

identifier :: [String] -> ParserM String
identifier ks = token $ do
  x <- ident
  if (x `elem` ks)
    then mzero
    else return x

identP = identifier ["hello", "world"]

test = runParserM $ parse . many . token $ identP


