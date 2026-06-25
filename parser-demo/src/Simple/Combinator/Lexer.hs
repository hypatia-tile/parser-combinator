module Simple.Combinator.Lexer (
    skipWhiteSpace,
    lexeme,
) where

import Control.Applicative
import Simple.Combinator.Parser

space :: (Monad m, Alternative m) => ParserT m () -> ParserT m ()
space sc = skipMany sc

skipMany :: (Monad m, Alternative m) => ParserT m a -> ParserT m ()
skipMany p = manyP (p *> return ()) *> return ()

skipWhiteSpace :: (Monad m, Alternative m, MonadFail m) => ParserT m ()
skipWhiteSpace = space skipSpaceP

lexeme :: (Monad m, Alternative m, MonadFail m) => ParserT m a -> ParserT m a
lexeme p = p <* skipWhiteSpace
