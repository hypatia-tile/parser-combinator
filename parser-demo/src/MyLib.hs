-- Create various parser here, experement and search my better way to write parser
module MyLib (someFunc) where

import SimpleLexer

someFunc :: IO ()
someFunc = test input

input :: String
input = "a + bcd * 12 + 3 * (45 + 6)"

test :: String -> IO ()
test src = do
  case lexArithmetic src of
    Left err -> putStrLn $ "Error: " ++ err
    Right tokens -> do
      putStrLn "Tokens:"
      mapM_ print tokens

