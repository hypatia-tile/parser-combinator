module Simple.Combinator.Parser (
    Parser,
    ParserT,
    runParser,
    mkParser,
    satisfy,
    satisfy1,
    char,
    takeWhileP,
    skipSpaceP,
    takeWhileP1,
    manyP,
) where

import Simple.Combinator.Parser.Internal
import Data.Char (isSpace)
import Control.Applicative


char :: (Monad m, MonadFail m) => Char -> ParserT m Char
char c = satisfy1 (c ==)

skipSpaceP :: (Monad m, MonadFail m) => ParserT m ()
skipSpaceP = satisfy1 (isSpace) *> return ()

