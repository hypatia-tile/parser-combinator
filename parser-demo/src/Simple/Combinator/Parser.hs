module Simple.Combinator.Parser (
    ParserT (runParser),
    mkParser,
) where

newtype ParserT m a = P {runParser :: String -> m (a, String)}

mkParser :: (String -> m (a, String)) -> ParserT m a
mkParser = P
