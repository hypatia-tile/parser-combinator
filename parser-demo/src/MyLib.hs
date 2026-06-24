-- Create various parser here, experement and search my better way to write parser
module MyLib (someFunc) where

import Simple.Parser.SimpleParser
import Simple.Combinator.Parser

someFunc :: IO ()
someFunc = test input

input :: String
input = "1+2+3*4"

test :: String -> IO ()
test src = do
  case runParser parseAExp src of
    Just expression -> print expression
    Nothing -> putStrLn "Failed to parse"

