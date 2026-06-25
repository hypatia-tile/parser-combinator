module Simple.Parser.SimpleParser (parseAExp) where

import Simple.Combinator.Parser
import Simple.Combinator.Lexer
import Simple.Data.AExp
import Data.Char (isDigit)

parseAExp :: ParserT Maybe (AExp Int)
parseAExp = expr

factor :: ParserT Maybe (AExp Int)
factor = lit_

term :: ParserT Maybe (AExp Int)
term = factor >>= (\l -> do rs <- manyP (mul_ *> factor); return $ foldl Mul l rs)

expr :: ParserT Maybe (AExp Int)
expr = term >>= (\l -> do rs <- manyP (add_ *> term); return $ foldl Add l rs)

lit_ :: ParserT Maybe (AExp Int)
lit_ = lexeme $ takeWhileP1 isDigit >>= (\digits -> return $ Lit . read $ digits)

mul_ :: ParserT Maybe ()
mul_ = lexeme $ char '*' *> return ()

add_ :: ParserT Maybe ()
add_ = lexeme $ char '+' *> return ()
