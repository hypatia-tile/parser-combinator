module Simple.Combinator.Parser (
    Parser,
    ParserT (runParser),
    mkParser,
    satisfy,
    char,
) where

import Data.Functor.Identity (Identity)

newtype ParserT m a = P {runParser :: String -> m (a, String)}

mkParser :: (String -> m (a, String)) -> ParserT m a
mkParser = P

type Parser a = ParserT Identity a

instance (Functor m) => Functor (ParserT m) where
    fmap f (P p) = P $ \inp -> fmap (\(x, y) -> (f x, y)) (p inp)

instance (Monad m) => Applicative (ParserT m) where
  pure x = P $ \inp -> pure (x, inp)
  (P f) <*> (P x) = P $ \inp -> do (f', res) <- f inp; (x', res') <- x res; return (f' x', res')

instance (Monad m) => Monad (ParserT m) where
  return = pure
  (P x) >>= f = P $ \inp -> do (x', res) <- x inp; runParser (f x') res

instance (MonadFail m) => MonadFail (ParserT m) where
  fail msg = P $ (\_ -> fail msg)

item :: (Monad m, MonadFail m) => ParserT m Char
item = P $ \inp -> case inp of
  [] -> fail "Nothing to Parse."
  x:res -> return (x, res)

satisfy :: (Monad m, MonadFail m) => (Char -> Bool) -> ParserT m Char
satisfy predic = item >>= (\c -> if predic c then return c else fail "Character does not satisfy the predicate.")

char :: (Monad m, MonadFail m) => Char -> ParserT m Char
char c = satisfy (c ==)
