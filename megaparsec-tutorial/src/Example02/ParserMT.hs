module Example02.ParserMT where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))

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

instance (Alternative m, Monad m) => Alternative (StateMonad s m) where
  empty = S $ \_ -> empty
  (S f) <|> (S g) = S $ \s -> f s <|> g s

instance (MonadPlus m) => MonadPlus (StateMonad s m) where
  mzero = empty
  mplus = (<|>)

type Pos = (Int, Int)
type Pstring = (Pos, String)
type ParserM a = StateMonad Pstring Maybe a


