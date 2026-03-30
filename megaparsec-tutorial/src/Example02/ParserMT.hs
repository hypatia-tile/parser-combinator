module Example02.ParserMT where

newtype ParserM m a = PMT { runPMT :: String -> m (a, String) }

type LexerM = ParserM Maybe

instance (Functor m) => Functor (ParserM m) where
  -- fmap (a -> b) -> ParserMT m a -> ParserMT m b
  fmap f p = PMT $ \inp -> fmap (\(x, rest) -> (f x, rest)) (runPMT p inp)

instance (Monad m) => Applicative (ParserM m) where
  -- pure :: m => a -> ParserMT a
  pure x = PMT $ \inp -> pure (x, inp)
  -- (<*>) :: ParserMT m (a -> b) -> ParserMT m a -> ParserMT m b
  (PMT f) <*> (PMT q) = PMT $ \inp -> do
    (f', s) <- f inp
    (q', s') <- q s
    return (f' q', s')

instance (Monad m) => Monad (ParserM m) where
  return = pure
  (PMT p) >>= f = PMT $ \inp -> do
    (v, res) <- p inp
    runPMT (f v) res

