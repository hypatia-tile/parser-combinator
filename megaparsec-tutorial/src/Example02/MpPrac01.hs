module Example02.MpPrac01 where

import Data.Void
import Text.Megaparsec
import qualified Data.Set as Set

type Parser = Parsec Void String

-- expect single character and report error with expected character if failed.
expecta :: Parser Char
expecta = single 'a'

testParser01 :: Parser Char
testParser01 = single 'a'

test01 :: (Show a) => Parser a -> String -> IO ()
test01 p inp = do
  parseTest p inp

test02 :: String -> String -> IO ()
test02 w inp = do
  parseTest (chunk w :: Parser String) inp

-- Parsec and ParsecT are monads,
-- and monadic bind is exactly what we use for sequencing our parsers.
mySequence :: Parser (Char, Char, Char)
mySequence = do
  a <- single 'a'
  b <- single 'b'
  c <- single 'c'
  return (a, b, c)

mySequence' :: Parser (Char, Char, Char)
mySequence' =
  (,,) <$>
    single 'a' <*>
    single 'b' <*>
    single 'c'

testP :: (Show a) => Parser a -> String -> IO ()
testP = parseTest
