module Simple.Parser.SimpleParser where

import Simple.Combinator.Parser
import Simple.Data.AExp
import Data.Char (isDigit)

expr :: ParserT Maybe (AExp Int)
expr = term >>= (\l -> do rs <- manyP (char '+' *> term); return $ foldl Add l rs)

term :: ParserT Maybe (AExp Int)
term = factor >>= (\l -> do rs <- manyP (char '*' *> factor); return $ foldl Mul l rs)

factor :: ParserT Maybe (AExp Int)
factor = takeWhileP1 isDigit >>= (\digits -> return $ Lit . read $ digits)
