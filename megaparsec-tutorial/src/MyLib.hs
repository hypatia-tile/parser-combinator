module MyLib (someFunc) where

import Control.Applicative (empty)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Set as Set

type Parser = Parsec Void String

testText :: Parser Char
testText = satisfy (== 'a')

runTest :: String -> Either (ParseErrorBundle String Void) Char
runTest input = parse testText "runTest" input

runTest2 :: String -> IO ()
runTest2 input =
  case runTest input of
    Left err -> putStr $ errorBundlePretty err
    Right c -> putStrLn $ "Parsed character: " ++ [c]

testText2 :: Parser Char
testText2 = token (\c ->
                     if c == 'a'
                       then Just c
                       else Nothing)
                  Set.empty
runTest3 :: String -> IO ()
runTest3 input =
  case runTest input of
    Left err -> putStr $ errorBundlePretty err
    Right c -> putStrLn $ "Parsed character: " ++ [c]

someFunc :: IO ()
someFunc = putStrLn "someFunc"
