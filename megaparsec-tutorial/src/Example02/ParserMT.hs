module Example02.ParserMT where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))

newtype ParserM m a = PMT {runPMT :: String -> m (a, String)}

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

instance (Alternative m, Monad m) => Alternative (ParserM m) where
  empty = PMT $ \_ -> empty
  (PMT p) <|> (PMT q) = PMT $ \inp ->
    p inp <|> q inp

instance (MonadPlus m, Monad m) => MonadPlus (ParserM m) where
  mzero = empty
  mplus = (<|>)

newtype StateMonad s m a = S { runState :: s -> m (a, s) }

instance (Functor m) => Functor (StateMonad s m) where
  fmap f (S st) = S $ fmap (\(x, st') -> (f x, st')) . st

instance (Monad m) => Applicative (StateMonad s m) where
  pure x = S $ \st -> pure (x, st)
  (S f) <*> (S st) = S $ \x -> do
    (u, s) <- f x
    (v, s') <- st s
    return (u v, s')

instance (Monad m) => Monad (StateMonad s m) where
  return = pure
  (S s) >>= f = S $ \st -> do
    (x, u) <- s st
    runState (f x) u

class (Monad m) => MonadState s m where
  fetch :: m s
  put :: s -> m ()
  state :: (s -> (a, s)) -> m a

update :: (MonadState s m) => (s -> s) -> m ()
update f = do
  x <- fetch
  _ <- put (f x)
  return ()

instance (Monad m) => MonadState s (StateMonad s m) where
  -- fetch :: StateMonad s m s
  fetch = S $ \s -> return (s, s)
  -- put :: s -> StateMonad s m ()
  put s = S $ \_ -> return ((), s)
  -- state :: (s -> (a, s)) -> StateMonad s m a
  state f = do
    s <- fetch
    let (v, s') = f s
    put s'
    return v

