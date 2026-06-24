module Simple.Parser.SimpleParser (parseAExp) where

import Simple.Combinator.Parser
import Simple.Data.AExp
import Data.Char (isDigit)

parseAExp :: ParserT Maybe (AExp Int)
parseAExp = expr

factor :: ParserT Maybe (AExp Int)
factor = takeWhileP1 isDigit >>= (\digits -> return $ Lit . read $ digits)

term :: ParserT Maybe (AExp Int)
term = factor >>= (\l -> do rs <- manyP (char '*' *> factor); return $ foldl Mul l rs)

expr :: ParserT Maybe (AExp Int)
expr = term >>= (\l -> do rs <- manyP (char '+' *> term); return $ foldl Add l rs)
