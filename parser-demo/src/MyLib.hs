-- Create various parser here, experement and search my better way to write parser
module MyLib (someFunc) where

import Simple.Parser.SimpleParser
import Simple.Combinator.Parser
import qualified Simple.Eval.AExp as A

someFunc :: IO ()
someFunc = test input

input :: String
input = "1 + 2 + 3 * 4 +    3 "

test :: String -> IO ()
test src = do
  case runParser parseAExp src of
    Just expression -> do
      print expression
      putStr "Val: "
      print . A.eval . fst $ expression
    Nothing -> putStrLn "Failed to parse"

