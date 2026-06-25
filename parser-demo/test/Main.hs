module Main (main) where

import Simple.Combinator.Parser (
    runParser,
    char,
    manyP,
    takeWhileP,
    takeWhileP1,
 )
import System.Exit (exitFailure)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
    if expected == actual
        then putStrLn ("[pass] " <> label)
        else do
            putStrLn ("[fail] " <> label)
            putStrLn ("  expected: " <> show expected)
            putStrLn ("  actual:   " <> show actual)
            exitFailure

main :: IO ()
main = do
  assertEqual
    "char succeeds on matching char"
    (Just ('+', "123"))
    (runParser (char '+') "+123")
  assertEqual
    "manyP char succeed on matching char"
    (Just ("----", "123"))
    (runParser (manyP $ char '-') "----123")
  assertEqual
    "manyP char succeed while no character match"
    (Just ("", "123"))
    (runParser (manyP $ char '-') "123")
  assertEqual
    "takeWhileP succeed on matching multi char"
    (Just ("111", "234"))
    (runParser (takeWhileP (== '1')) "111234")
  assertEqual
    "takeWhileP succeed while no character match"
    (Just ("", "234"))
    (runParser (takeWhileP (== '1')) "234")
  assertEqual
    "takeWhileP1 succeed on matching multi char"
    (Just ("111", "234"))
    (runParser (takeWhileP1 (== '1')) "111234")
  assertEqual
    "takeWhileP1 fail if no character match"
    (Nothing)
    (runParser (takeWhileP1 (== '1')) "234")
