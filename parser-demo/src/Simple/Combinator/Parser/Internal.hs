module Simple.Combinator.Parser.Internal where

import Control.Applicative
import Data.Functor.Identity (Identity)

newtype ParserT m a = P {unParser :: State -> m (Reply a)}

runParser :: (Monad m, MonadFail m) => ParserT m a -> String -> m (a, String)
runParser (P p) source = do
    Rep st rt _ <- p (initialState source)
    case rt of
        OK x -> return (x, restInput st)
        Error msg -> fail msg

data Reply a = Rep
    { rState :: State
    , rResult :: Result a
    , rConsumption :: Consumption
    }
    deriving (Show)

instance Functor Reply where
    fmap f a = let x = rResult a in a{rResult = fmap f x}

data Consumption
    = Consumed
    | NotConsumed
    deriving (Show)

instance Semigroup Consumption where
    Consumed <> _ = Consumed
    NotConsumed <> a = a
instance Monoid Consumption where
    mempty = NotConsumed

data ParseError
    = UnknownError
    | UnexpectedEof
    deriving (Show)

data State = S
    { stateInput :: String
    , stateOffset :: !Int
    , stateTotalLen :: !Int
    , stateParseErrors :: [ParseError]
    }
    deriving (Show)

initialState :: String -> State
initialState source = S source 0 (length source) []

restInput :: State -> String
restInput st = drop (stateOffset st) (stateInput st)

setOffset :: Int -> State -> State
setOffset n st = st{stateOffset = n}

addError :: ParseError -> State -> State
addError err st = st{stateParseErrors = err : stateParseErrors st}

advance :: State -> Maybe (Char, State)
advance state@(S inp off len err)
    | off < len = Just (inp !! off, setOffset (off + 1) state)
    | otherwise = Nothing

data Result a
    = OK a
    | Error String
    deriving (Show)

instance Functor Result where
    fmap f a = case a of
        OK x -> OK (f x)
        Error msg -> Error msg

instance Applicative Result where
    pure x = OK x
    (OK f) <*> (OK x) = OK (f x)
    (OK _) <*> (Error msg) = Error msg
    (Error msg) <*> _ = Error msg

mkParser :: (State -> m (Reply a)) -> ParserT m a
mkParser = P

type Parser a = ParserT Identity a

instance (Functor m) => Functor (ParserT m) where
    fmap f (P p) = P $ \inp -> fmap (fmap f) (p inp)

instance (Monad m) => Applicative (ParserT m) where
    pure x = P $ \inp -> pure (Rep{rState = inp, rResult = pure x, rConsumption = NotConsumed})
    (P f) <*> (P x) = P $ \inp -> do
        Rep st1 rt1 con1 <- f inp
        Rep st2 rt2 con2 <- x st1
        return $ Rep st2 (rt1 <*> rt2) (con1 <> con2)

instance (Monad m) => Monad (ParserT m) where
    return = pure
    (P x) >>= f = P $ \inp -> do
        Rep st result con <- x inp
        case result of
            OK x' -> do
                y <- unParser (f x') st
                return y{rConsumption = con <> rConsumption y}
            Error msg -> return (Rep st (Error msg) con)

instance (Monad m, Alternative m) => Alternative (ParserT m) where
    empty = P $ \_ -> empty
    (P p) <|> (P q) = P $ \inp -> p inp <|> q inp

instance (MonadFail m) => MonadFail (ParserT m) where
    fail msg = P $ (\_ -> fail msg)

-- | advance with consumption of input
item1 :: (Monad m) => ParserT m Char
item1 = P $ \inputState -> case advance inputState of
    Just (c, state) -> return $ Rep state (pure c) Consumed
    Nothing -> return $ Rep (addError UnexpectedEof inputState) (Error "Unexpected EOF") Consumed

-- | Uncousuming advance
item :: (Monad m) => ParserT m Char
item = P $ \inputState -> case advance inputState of
    Just (c, state) -> return $ Rep state (OK c) NotConsumed
    Nothing -> return $ Rep inputState (Error "Unexpected EOF") NotConsumed

satisfy1 :: (Monad m) => (Char -> Bool) -> ParserT m Char
satisfy1 predic = P $ \inputState -> case advance inputState of
    Just (c, state) ->
        if predic c
            then return $ Rep state (OK c) Consumed
            else return $ Rep state (Error "char does not satisfy condition") Consumed
    Nothing -> return $ Rep (addError UnexpectedEof inputState) (Error "Unexpected EOF") Consumed

satisfy :: (Monad m) => (Char -> Bool) -> ParserT m Char
satisfy predic = P $ \inputState -> case advance inputState of
    Just (c, state) ->
        if predic c
            then return $ Rep state (OK c) Consumed
            else return $ Rep state (Error "char does not satisfy condition") NotConsumed
    Nothing -> return $ Rep inputState (Error "Unexpected EOF") NotConsumed

takeWhileP :: (Monad m) => (Char -> Bool) -> ParserT m [Char]
takeWhileP predic = P $ \inputState -> case advance inputState of
    Just (c, state) ->
        if predic c
            then do
                Rep newState rt _ <- unParser (takeWhileP predic) state
                case rt of
                    OK cs -> return $ Rep newState (OK (c : cs)) Consumed
                    Error msg -> error $ "Unreachable: " <> msg
            else return $ Rep inputState (OK []) NotConsumed
    Nothing -> return $ Rep inputState (Error "Unexpected EOF") NotConsumed

takeWhileP1 :: (Monad m, MonadFail m) => (Char -> Bool) -> ParserT m [Char]
takeWhileP1 predic = do
    l <- satisfy1 predic
    r <- takeWhileP predic
    return (l : r)

manyP :: (Monad m, Alternative m) => ParserT m a -> ParserT m [a]
manyP (P p) = P $ \inputState -> do
    Rep state result con <- p inputState
    case result of
        Error _ -> return $ Rep inputState (OK []) NotConsumed
        OK x -> case con of
            NotConsumed -> error "Unconsuming parser cannot be passed to manyP"
            Consumed -> do
                Rep nState result' _ <- unParser (manyP (P p)) state
                case result' of
                    OK xs -> return $ Rep nState (OK (x : xs)) Consumed
                    Error msg -> error $ "Unreachable: " <> msg
