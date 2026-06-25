module Simple.Combinator.Parser (
    Parser,
    ParserT (runParser),
    mkParser,
    satisfy,
    char,
    takeWhileP,
    skipSpaceP,
    takeWhileP1,
    manyP,
) where

import Data.Functor.Identity (Identity)
import Data.Char (isSpace)
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

state :: (Monad m) => ParserT m String
state = P $ \inp -> return (inp, inp)

put :: (Monad m) => String -> ParserT m ()
put st = P $ \_ -> return ((), st)

item :: (Monad m, MonadFail m) => ParserT m Char
item = P $ \inp -> case inp of
  [] -> fail "Nothing to Parse."
  x:res -> return (x, res)

item_ :: (Monad m) => ParserT m (Maybe Char)
item_ = P $ \inp -> case inp of
  [] -> return (Nothing, inp)
  x:res -> return (Just x, res)

satisfy :: (Monad m, MonadFail m) => (Char -> Bool) -> ParserT m Char
satisfy predic = item >>= (\c -> if predic c then return c else fail "Character does not satisfy the predicate.")

char :: (Monad m, MonadFail m) => Char -> ParserT m Char
char c = satisfy (c ==)

satisfy_ :: (Monad m) => (Char -> Bool) -> ParserT m (Maybe Char)
satisfy_ predic = item_ >>= (go predic)
  where
    go :: (Monad m) => (Char -> Bool) -> Maybe Char -> ParserT m (Maybe Char)
    go p x = P $ \inp -> case x of
      Just x' -> if p x' then return (Just x', inp) else return (Nothing, inp)
      _ -> return (Nothing, inp)

skipSpaceP :: (Monad m, MonadFail m) => ParserT m ()
skipSpaceP = satisfy (isSpace) *> return ()

takeWhileP :: (Monad m) => (Char -> Bool) -> ParserT m [Char]
takeWhileP predic = let
    oneStep = satisfy_ predic
  in do
    st <- state
    x <- oneStep
    case x of
      Nothing -> do
        put st
        return []
      Just x' -> do
        (x' :) <$> takeWhileP predic

takeWhileP1 :: (Monad m, MonadFail m) => (Char -> Bool) -> ParserT m [Char]
takeWhileP1 predic = do
  l <- satisfy predic
  r <- takeWhileP predic
  return (l:r)

manyP :: (Monad m, Alternative m) => ParserT m a -> ParserT m [a]
manyP p = (p >>= go (manyP p)) <|> return []
  where
    go :: (Monad m, Alternative m) => ParserT m [a] -> a -> ParserT m [a]
    go q = \x -> do xs <- q <|> return []; return (x:xs)
