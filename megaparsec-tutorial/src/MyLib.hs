module MyLib (someFunc) where

import Example02.MpPrac01

someFunc :: IO ()
someFunc = do
  test01 testParser01 "allow"
  test01 testParser01 "ban"

