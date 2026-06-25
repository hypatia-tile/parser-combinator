module Simple.Combinator.Parser.Internal where

import Data.Functor.Identity (Identity)
import Control.Applicative

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

instance (Monad m, Alternative m) => Alternative (ParserT m) where
  empty = P $ \_ -> empty
  (P p) <|> (P q) = P $ \inp -> p inp <|> q inp

instance (MonadFail m) => MonadFail (ParserT m) where
  fail msg = P $ (\_ -> fail msg)

